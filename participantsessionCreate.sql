CREATE TABLE participantsession(
id INT UNIQUE PRIMARY KEY AUTO_INCREMENT, 
studyCode VARCHAR(500),
hearingStatus VARCHAR(500),
ageAcqASL INT,
age INT,
majorReq VARCHAR(500),
whyASL VARCHAR(500),
major VARCHAR(500),
nativeLang VARCHAR(500),
ageAcqEng INT,
langs VARCHAR(500),
gAnalyticsID VARCHAR(500),
startTime VARCHAR(500)
)
