ui requests
warningDefaultAction: aWarning
	| logBlock |
	"Pass all warnings, but log them"
	
	logBlock := [:logger |
		logger 
			cr;
			nextPutAll: '*** Warning: ';
			nextPutAll: aWarning description;
			cr ].
	
	Smalltalk logDuring: logBlock.
	self logYellowDuring: logBlock.
	
	aWarning resume.

	