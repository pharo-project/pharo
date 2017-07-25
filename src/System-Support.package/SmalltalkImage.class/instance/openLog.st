miscellaneous
openLog
	"This is a _private_ method,
	Because it really belongs to logging facility,
	we should delegate to it at some point "

	^ ZnCharacterWriteStream
		on:
			((File named: Smalltalk logFileName) writeStream
				setToEnd;
				yourself)
		encoding: 'utf8'