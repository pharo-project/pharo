selecting
selection: ann
	| selection |
	selection := ann newSelectedRowIndexes
		select: [ :each | each ~= 0 ]
		thenCollect: [ :each | (self elementAt: each) data ].
	selection
		ifNotEmpty: [ 
			self announcer
				announce:
					(GLMTreeMorphSelectionChanged new
						selectionValue:
								(selection size = 1
										ifTrue: [ selection first ]
										ifFalse: [ selection ]);
						selectionPathValue: selection;
						yourself) ]