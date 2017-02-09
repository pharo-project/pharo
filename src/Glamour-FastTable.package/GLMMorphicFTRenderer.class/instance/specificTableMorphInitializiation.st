initialization
specificTableMorphInitializiation
	"Override me if need to add something to the tableMorph."

	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph rowHeight: presentation rowHeight.
	"I set the right function."
	presentation isSearchable
		ifTrue: [ tableMorph enableSearch ].
	presentation isFilterable
		ifTrue: [ presentation needActionButton
				ifTrue: [ tableMorph enableFilterWithAction: presentation actionBlock named: presentation actionName ]
				ifFalse: [ tableMorph enableFilter ] ].
	(presentation isFilterable or: [ presentation isSearchable ])
		ifFalse: [ tableMorph disableFunction ]
		ifTrue: [ tableMorph explicitFunction ]