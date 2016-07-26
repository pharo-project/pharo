logging-Deprecated
logCr: aString
	"Log the argument. Use self logCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	Transcript show: aString ; cr