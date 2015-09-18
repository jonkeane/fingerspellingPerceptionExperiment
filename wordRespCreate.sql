CREATE TABLE wordResp(
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
FOREIGN KEY (partsessionid) 
        REFERENCES participantsession(id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
)
