CREATE TABLE captchASL(
id INT UNIQUE PRIMARY KEY AUTO_INCREMENT, 
gAnalyticsID VARCHAR(500),
video VARCHAR(500),
response VARCHAR(500),
correct TINYINT(1)
)