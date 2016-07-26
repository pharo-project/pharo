miscellaneous
openLog
	"This is a _private_ method,
	Because it really belongs to logging facility,
	we should delegate to it at some point "

	^ (FileStream fileNamed: Smalltalk logFileName)
		ifNil: [ NullStream new ]
		ifNotNil: [ :stream | 
			stream
				wantsLineEndConversion: true;
				setToEnd;
				yourself ]