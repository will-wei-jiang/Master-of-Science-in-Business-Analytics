# create and use the schema
CREATE SCHEMA netflix;
USE netflix;

# create tables
CREATE TABLE title_id_rating (
tconst TEXT,
avg_rating FLOAT,
num_votes INT,
title VARCHAR(125),
id TEXT,
CONSTRAINT pk_rating PRIMARY KEY (title));

CREATE TABLE netflix_original_movies (
title VARCHAR(125),
genre TEXT,
runtime TEXT,
lang TEXT,
CONSTRAINT pk_original PRIMARY KEY (title));

CREATE TABLE netflix_all_information (
title VARCHAR(125),
country TEXT,
date_added DATE,
release_year INT,
cate_rating TEXT,
duration TEXT,
genre TEXT,
CONSTRAINT pk_allinfo PRIMARY KEY (title));

# import csv data to the tables created above
SET GLOBAL local_infile = 1; #allow local_infile

LOAD DATA LOCAL INFILE 'C:/Users/Will Jiang/Desktop/Emory Desktop/Big Data/Final Project/data/title_id_rating.csv' # need to change file path here to load data
  INTO TABLE title_id_rating
  FIELDS TERMINATED BY ','
  ENCLOSED BY '"'
  LINES TERMINATED BY '\n'
  IGNORE 1 ROWS; # we already have head lines
  
LOAD DATA LOCAL INFILE 'C:/Users/Will Jiang/Desktop/Emory Desktop/Big Data/Final Project/data/netflix_original_movies.csv' # need to change file path here to load data
  INTO TABLE netflix_original_movies
  FIELDS TERMINATED BY ','
  ENCLOSED BY '"'
  LINES TERMINATED BY '\n'
  IGNORE 1 ROWS; # we already have head lines
  
LOAD DATA LOCAL INFILE 'C:/Users/Will Jiang/Desktop/Emory Desktop/Big Data/Final Project/data/netflix_all_information.csv' # need to change file path here to load data
  INTO TABLE netflix_all_information
  CHARACTER SET latin1 # there are import error since film titles is really messy
  FIELDS TERMINATED BY ','
  ENCLOSED BY '"'
  LINES TERMINATED BY '\n'
  IGNORE 1 ROWS # we already have head lines
  (title,country,@date_added,release_year,cate_rating,duration,genre)
  SET date_added = STR_TO_DATE(@date_added, '%d-%M-%y');
  
# combine all three tables based on film title, and create a new table contain all the needed information
CREATE TABLE netflix_clean
AS
(
SELECT i.*, o.lang, r.avg_rating, r.num_votes
FROM netflix_all_information i 
LEFT JOIN netflix_original_movies o ON i.title = o.title # use left join here so we can tell which movie is netflix original and which is not in the following cleaning steps
INNER JOIN title_id_rating r ON i.title = r.title # only match movie with rating since it's also target variable when building predictive models
);

# see how many rows left
SELECT COUNT(*) FROM netflix_clean; #4210

# create a new column regarding whetehr a movie is netflix original or not
ALTER TABLE netflix_clean ADD COLUMN is_original INT;
SET SQL_SAFE_UPDATES = 0; # disable safe update mode
UPDATE netflix_clean SET is_original = 1 WHERE lang IS NOT NULL; #if column lag have data, then iy comes from original table, then this movie is netflix original
UPDATE netflix_clean SET is_original = 0 WHERE lang IS NULL;

# delete column lang since it's useless now
ALTER TABLE netflix_clean DROP COLUMN lang;

# create a new column regarding whetehr a movie is made in US or not
ALTER TABLE netflix_clean ADD COLUMN is_US INT;
UPDATE netflix_clean SET is_US = 1 WHERE country = 'United States';
UPDATE netflix_clean SET is_US = 0 WHERE country != 'United States' AND country != 'NA';

# create a new column regarding in which season this film is added to netflix platform
ALTER TABLE netflix_clean ADD COLUMN season_added TEXT;
UPDATE netflix_clean SET season_added = 'spring' WHERE MONTH(date_added) IN (3,4,5); # spring is from month 3 to 5
UPDATE netflix_clean SET season_added = 'summer' WHERE MONTH(date_added) IN (6,7,8); # summer is from month 6 to 8
UPDATE netflix_clean SET season_added = 'fall' WHERE MONTH(date_added) IN (9,10,11); # fall is from month 9 to 11
UPDATE netflix_clean SET season_added = 'winter' WHERE MONTH(date_added) IN (12,1,2); # winter is from month 12 to 2

# change duration data formate to integer from text
UPDATE netflix_clean SET duration = TRIM(REPLACE(duration,'min',''));

# see how many catetory rating (different from audience vote rating) there is 
SELECT DISTINCT(cate_rating) FROM netflix_clean;
DELETE FROM netflix_clean WHERE cate_rating IN ('74 min','84 min','66 min'); # delete three rows that cate_rating is messy

# create a new column regarding whetehr parent guidance is suggested for children to watch a movie
ALTER TABLE netflix_clean ADD COLUMN audience_class TEXT;
UPDATE netflix_clean SET audience_class = 1 WHERE cate_rating IN ('G','TV-Y','TV-Y7','TV-Y7-FV','TV-G'); # audience for this class can be young and they do not need parent guidence when watching the films
UPDATE netflix_clean SET audience_class = 2 WHERE cate_rating IN ('PG','PG-13','R','TV-PG','TV-14'); # audience for this class cen be young but they need parent guidence when watching the films
UPDATE netflix_clean SET audience_class = 3 WHERE cate_rating IN ('NC-17','TV-MA'); # audience for this class should only be adult

# check genres
SELECT DISTINCT genre, COUNT(*) FROM netflix_clean GROUP BY genre;
SELECT * FROM netflix_clean WHERE genre = 'Movies'; # there are some films that their genre is Movies which is meaningless
DELETE FROM netflix_clean WHERE genre = 'Movies'; # delete three rows that genre is meaningless
# we will use genres to generate dummy variables, but it's much more easier to do so in Python directly, so we will skip this step here in MySQL

# we need completed data without NAs when bulid predictive models, so we need to delete data with NAs
DELETE FROM netflix_clean WHERE country = 'NA'; # delete rows that country is NA
DELETE FROM netflix_clean WHERE audience_class IS NULL; # delete rows that audience class is null (cate_rating is NR/UR which means not rated)

# see final cleaned table
SELECT * FROM netflix_clean;

# export data to csv file
SELECT @@secure_file_priv; # there is secure-file-priv restrict, so need to check the file path that can be exported

# cannot include headers directly, need to hard code
SELECT 'title','country','date_added','release_year','cate_rating','duration','genre','avg_rating','num_votes','is_original','is_US','season_added','audience_class'
UNION ALL
SELECT * FROM netflix_clean
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/netflix_clean.csv'
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n';







