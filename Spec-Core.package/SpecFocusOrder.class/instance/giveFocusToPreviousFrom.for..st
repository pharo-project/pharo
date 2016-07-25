as yet unclassified
giveFocusToPreviousFrom: aPresenter for: aModel
	| focus |
	focus := self presenters.
	(focus includes: aPresenter)
		ifTrue: [ | index previous |
			index := (focus indexOf: aPresenter) - 1.
			index > 0
				ifTrue: [ previous := focus at: index.
					previous takeLastKeyboardFocus.
					^ true ] ].
	^ aModel owner
		ifNil: [ | next |
			next := focus at: focus size ifAbsent: [ ^ false ].
			next takeLastKeyboardFocus.
			true ]
		ifNotNil: [ aModel owner giveFocusToPreviousFrom: aModel.
			true ]	" I loop "