mouse managing
mouseDown: event on: aTargetMorph
	"Changed to take keybaord focus."
	| clickedTheCheckbox |
	
	clickedTheCheckbox := self clicked: event inTheCheckboxOf: aTargetMorph.
	
	(self autoMultiSelection and: [ event shiftPressed not ])
		ifTrue: [ 
			firstClickedMorph := aTargetMorph.
			aTargetMorph selected
				ifTrue: [ 
					self removeFromSelection: aTargetMorph.
					(clickedTheCheckbox not and: [ self selectOnlyLastHighlighted and: [ aTargetMorph hasContentToShow ] ])
						ifTrue: [ 
							aTargetMorph = self lastClickedMorph
								ifFalse: [ self addToSelection: aTargetMorph ] ] ]
				ifFalse: [ 
					(clickedTheCheckbox not and: [ self selectOnlyLastHighlighted and: [ aTargetMorph hasContentToShow ] ])
						ifTrue: [ 
							aTargetMorph = self lastClickedMorph
								ifTrue: [ 
									self addToSelection: aTargetMorph.
									clickedTheCheckbox
										ifFalse: [ self lastClickedMorph: aTargetMorph ] ] ]
						ifFalse: [ 
							self addToSelection: aTargetMorph.
							clickedTheCheckbox
								ifFalse: [ self lastClickedMorph: aTargetMorph ] ] ] ].
	
	(clickedTheCheckbox not and: [ event shiftPressed not or: [ firstClickedMorph isNil ] ])
		ifTrue: [ firstClickedMorph := aTargetMorph ].
	
	aTargetMorph mouseDown: event