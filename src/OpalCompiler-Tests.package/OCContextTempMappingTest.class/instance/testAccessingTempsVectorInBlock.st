tests
testAccessingTempsVectorInBlock

	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].

	[ | temp |
		[ temp := 1.
		self assert: ((thisContext tempNamed: 'temp') == 1) ] value.
	] value