import mysql.connector

db = mysql.connector.connect(
    host='34.138.188.252',
    user='root',
    passwd='uconnstamford',
    database='DATA'
    )

mycursor = db.cursor()

##create the database
##this should only be executed before the creation of the DATA database

#mycursor.execute("CREATE DATABASE DATA")


##create table to input merged_data.csv
##this should only be executed once
#str_ = 'CREATE TABLE Stock (isinID VARCHAR(40) NOT NULL, ticker VARCHAR(50) PRIMARY KEY, Month smallint CHECK(Month BETWEEN 1 AND 12), Year int, price float(8,2), industryAdjustedScore float(5,2), weightedAvgScore float(5,2), environmentalPillarScore float(5,2), goverancePillarScore float(5,2), socialPillarScore float(5,2))'

#mycursor.execute(str_)


