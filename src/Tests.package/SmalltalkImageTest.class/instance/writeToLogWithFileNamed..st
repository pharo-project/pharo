helpers
writeToLogWithFileNamed: filename
	| oldLogFileName |
	oldLogFileName := Smalltalk logFileName.
	Smalltalk logFileName: filename.
	[ 
	| stream |
	stream := Smalltalk image openLog.
	stream nextPutAll: 'foobar'	"We just make sure we can write something. I don't know any reliable way to check if it has really been written" ]
		ensure: [ Smalltalk logFileName: oldLogFileName ]