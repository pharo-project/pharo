results
afterHooks
	| statements |
	statements := links 
			select: [ :each | each control = #after ] 
			thenCollect:  [ :link | (self hookFor: link) parent: node].
			
	^(RBBlockNode body: (RBSequenceNode statements: statements flattened)) parent: node.