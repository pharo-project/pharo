tests
testIsA
	| nodes types |
	nodes := Bag new.
	types := Set new.
	#(#(#isAssignment 1) #(#isBlock 1) #(#isCascade 1) #(#isLiteralNode 2) #(#isMessage 3) #(#isMethod 1) #(#isReturn 1) #(#isSequence 2) #(#isValue 15) #(#isVariable 7) #(#isUsed 10) #(#isDirectlyUsed 9) #(#hasParentheses 1) #(#isBinary 0) #(#isPrimitive 0) #(#isImmediateNode 10) #(#isWrite 1) #(#isRead 3)) 
		do: 
			[:each | 
			each last timesRepeat: [nodes add: each first].
			types add: each first].
	self treeWithEverything nodesDo: 
			[:each | 
			types do: 
					[:sel | 
					((each respondsTo: sel) and: [each perform: sel]) 
						ifTrue: [nodes remove: sel]]].
	self assert: nodes isEmpty