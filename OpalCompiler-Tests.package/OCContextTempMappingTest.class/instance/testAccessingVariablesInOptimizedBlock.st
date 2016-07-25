tests
testAccessingVariablesInOptimizedBlock
	
	1 to: 2 do: [ :index | |optimizedTemp|
		optimizedTemp := index.
		self assert: (thisContext tempNamed: 'optimizedTemp') == index
	]