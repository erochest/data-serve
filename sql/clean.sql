
DELETE FROM internet_users
WHERE year IN
('1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969',
 '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979');

SELECT COUNT(*) FROM internet_users;

