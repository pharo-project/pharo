tests
testAccessingVariablesInOptimizedBlock2

	| a b |

	a:=1.
	b:=2.
	
	a < b ifTrue:[ |c|
		c:=50.
		self assert: (thisContext tempNamed: 'a') == 1.
		self assert: (thisContext tempNamed: 'b') == 2.
		self assert: (thisContext tempNamed: 'c') == 50.
	].