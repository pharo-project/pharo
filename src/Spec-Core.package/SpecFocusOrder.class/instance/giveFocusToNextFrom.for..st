as yet unclassified
giveFocusToNextFrom: aPresenter for: aModel
	"aPresenter is the object what currently get the focus, aModel is the object use to call the good parent in a complex hierarchy"

	| focus |
	focus := self presenters.
	(focus includes: aPresenter)
		ifTrue: [ | index next |
			index := (focus indexOf: aPresenter) + 1.
			index <= focus size
				ifTrue: [ next := focus at: index.
					next takeKeyboardFocus.
					^ true ] ].
	^ aModel owner
		ifNil: [ | next |
			next := focus at: 1 ifAbsent: [ ^ false ].
			next takeKeyboardFocus.
			true ]
		ifNotNil: [ aModel owner giveFocusToNextFrom: aModel.
			true ]	"loop if there is no parent, else give it the focus"