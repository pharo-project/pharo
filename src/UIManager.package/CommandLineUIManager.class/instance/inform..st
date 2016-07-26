ui requests
inform: aString

	| logBlock |

	"Just log notifications"
	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer | ^true].
	
	logBlock := [:logger |
		logger cr;
			nextPutAll: (String new: 79 withAll: $= ); cr;
			nextPutAll: 'Notice: ';
			nextPutAll: aString; cr;
			nextPutAll: (String new: 79 withAll: $= ); cr].

	Smalltalk logDuring: logBlock.		
	aString logCr.