go to Install.R and Source
go to the FD contest and click the "Download players list".  Rename the file salary.csv and move to the ./Data/ directory
go to Config.R
	1) make sure week is set correctly
	2) set noise to the desired amount

Select a defense.  I use http://www.fantasypros.com/nfl/rankings/dst.php.  Once you have selected a defense run the following command 
salary[salary$Position == "D",]
Take the id of the team and make sure it is in the USE_PLAYER list.

go to Main.R and click Source.  The code should run and produce a lineup. 

