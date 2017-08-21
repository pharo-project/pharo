tests
testAccessingVariablesInBlock
	| theContext |

	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].

	[ | t1 |
		t1 := true.
		theContext := thisContext.
		self assert: (thisContext tempNamed: 'theContext') == thisContext.
		self assert: (theContext tempNamed: 't1').
	] value
	