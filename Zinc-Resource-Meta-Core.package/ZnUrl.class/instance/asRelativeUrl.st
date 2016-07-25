converting
asRelativeUrl
	"Copy the receiver, with scheme, host and port cleared"
	
	^ self copy
		scheme: nil;
		host: nil;
		port: nil;
		yourself