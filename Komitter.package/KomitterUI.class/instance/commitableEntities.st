private
commitableEntities
	| map |
	
	map := Dictionary new.
	
	tree roots do: [ :e || nodes | 
		e content isFullyCommited: e selected.
		nodes := e allSelectedItems
			collect: [ :each | each content ]
			thenSelect: [ :each | each isCommitable ].
		nodes ifNotEmpty: [ map at: e content put: nodes ] ].

	^ map