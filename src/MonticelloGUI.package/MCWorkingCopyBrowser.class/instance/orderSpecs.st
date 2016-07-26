morphic ui
orderSpecs
	^ {
		'Sort alphabetically' -> [ :a :b | a package name <= b package name ].
		'Sort dirty first' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ a needsSaving ] ].
		'Sort dirty last' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ b needsSaving ] ].
		'Only dirty' -> [ :a :b | a package name <= b package name ]
	}