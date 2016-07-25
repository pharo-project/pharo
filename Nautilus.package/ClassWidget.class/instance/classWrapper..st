private list model
classWrapper: anElement
	^ self sortHierarchically
		ifTrue: [ self buildTabbedNameOf: anElement ]
		ifFalse: [  self wrapFlat: anElement  ]