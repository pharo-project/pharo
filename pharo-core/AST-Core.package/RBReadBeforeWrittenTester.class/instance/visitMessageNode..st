visiting
visitMessageNode: aMessageNode 
	((#(#whileTrue: #whileFalse: #whileTrue #whileFalse) 
		includes: aMessageNode selector) and: [aMessageNode receiver isBlock]) 
		ifTrue: [self executeTree: aMessageNode receiver body]
		ifFalse: 
			[(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded]) 
				ifTrue: [self visitNode: aMessageNode receiver]].
	((#(#ifTrue:ifFalse: #ifFalse:ifTrue:) includes: aMessageNode selector) 
		and: [aMessageNode arguments allSatisfy: [:each | each isBlock]]) 
			ifTrue: [^self processIfTrueIfFalse: aMessageNode].
	aMessageNode arguments do: [:each | self visitNode: each]