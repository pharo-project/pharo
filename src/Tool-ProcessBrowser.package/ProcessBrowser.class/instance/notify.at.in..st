process list
notify: errorString at: location in: aStream 
	"A syntax error happened when I was trying to highlight my pc. 
	Raise a signal so that it can be ignored."
	Warning signal: 'syntax error'