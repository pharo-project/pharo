running
setUp

	| aFile |
	
	progressFile nextPutAll: 'running suite: ';
		nextPutAll: suite name ; crlf; flush.
		
	aFile := File named: suite name , '-Test.xml'.
	aFile delete.
	stream := ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8'.	
		
	stream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; lf.
	stream nextPutAll: '<testsuite name="'; nextPutAll: (self encode: suite name); 
		nextPutAll: '" tests="'; print: suite tests size; nextPutAll: '">'.
	
	"Now this is ugly. We want to update the time and the number of failures and errors, but still at the same time stream a valid XML. So remember this position and add some whitespace, that we can fill later."
	suitePosition := stream wrappedStream position - 1.
	stream nextPutAll: (String new: 100 withAll: $ ); lf.
	
	"Initialize the test resources."
	suite resources do: [ :each |
		each isAvailable
			ifFalse: [ each signalInitializationError ] ]