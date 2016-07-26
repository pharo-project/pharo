printing
mmddyyyy
	"Print the receiver in standard U.S.A format mm/dd/yyyy.
	Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, 
	so that for example February 1 1996 is 2/1/96"

	^ self printFormat: #(2 1 3 $/ 1 1)