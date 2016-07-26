private
uncommittedCommitableEntitied
	| map |
	
	map := Dictionary new.
	
	tree roots 
		select: [ :e | e isPartialMatch ]
		thenDo: [ :e || nodes | 
			nodes := e allUnselectedItems
				collect: [ :each | each content ]
				thenSelect: [ :each | each isCommitable ].
			nodes ifNotEmpty: [ map at: e content put: nodes ] ].

	^ map