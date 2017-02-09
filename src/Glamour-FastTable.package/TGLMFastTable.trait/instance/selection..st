selecting
selection: ann
	| selection |
	selection := ann newSelectedRowIndexes select: [ :each | each ~= 0 ] thenCollect: [ :each | self elementAt: each ].
	selection
		ifNotEmpty: [ 
			self announcer
				announce:
					(GLMTreeMorphSelectionChanged new
						selectionValue:
								((selection size = 1 and: [ self glamourPresentation isSingle ])
										ifTrue: [ selection first ]
										ifFalse: [ selection ]);
						selectionPathValue: selection;
						yourself) ]