accessing
at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'