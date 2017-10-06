tests
testAccessingVariablesInOptimizedBlock
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].

	1 to: 2 do: [ :index | |optimizedTemp|
		optimizedTemp := index.
		self assert: (thisContext tempNamed: 'optimizedTemp') == index
	]