logging-Deprecated
logCrTab: aString
	"Log the argument. Use self logCr: 'something' instead of Transcript show: 'something'  ; cr ; tab"
	
	Transcript show: aString ; cr ; tab