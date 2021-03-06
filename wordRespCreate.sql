CREATE TABLE wordResp(
id INT UNIQUE PRIMARY KEY AUTO_INCREMENT, 
partsessionid INT, 
word VARCHAR(500), 
timestamp VARCHAR(500), 
video VARCHAR(500), 
numInBlock INT, 
block VARCHAR(500), 
gAnalyticsID VARCHAR(500), 
repetitions INT,
speed VARCHAR(500), 
maskcolor VARCHAR(500),
masktype VARCHAR(500),
playCount INT, 
FOREIGN KEY (partsessionid) 
        REFERENCES participantsession(id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
)
