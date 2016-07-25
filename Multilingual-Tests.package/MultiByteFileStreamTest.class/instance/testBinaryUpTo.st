testing
testBinaryUpTo
	"This is a non regression test for bug http://bugs.squeak.org/view.php?id=6933"
	"self run: #testBinaryUpTo"
	
	| foo fileName |
	
	fileName := 'foobug6933'. 
	MultiByteFileStream 
		forceNewFileNamed: fileName 
		do: [ :stream | stream binary.
				stream nextPutAll: #[1 2 3 4] ].

	foo := MultiByteFileStream oldFileNamed: fileName.
	[foo binary.
		self assert: (foo upTo: 3) = #[1 2] 
	] ensure: [
		foo close.
 		fileName  asFileReference delete]. 
    