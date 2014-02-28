CREATE TABLE IF NOT EXISTS internet_users
(
    id           SERIAL      PRIMARY KEY,
    country      VARCHAR(60) NOT NULL,
    country_code CHAR(5)     NOT NULL,
    ser_code     VARCHAR(15) NOT NULL,
    ser_name     VARCHAR(40) NOT NULL,
    year         CHAR(4)     NOT NULL,
    value        INTEGER     DEFAULT NULL
);

COPY internet_users
FROM '/Users/err8n/p/data-serve/internet-users.csv'
(FORMAT csv, HEADER true);

