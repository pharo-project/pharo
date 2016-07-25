interpreting-private
retrieveSpecFrom: aPresenter selector: aSelector

	((self model respondsTo: #needRebuild) and: [ self model needRebuild not and: [ self model spec notNil ] ])
		ifTrue: [
			spec := self model spec.
			self model needRebuild: true.
			((spec respondsTo: #isRedrawable) and: [ spec instance isSpecAdapter ])
				ifFalse: [ spec := self computeSpecFrom: arrayToInterpret first selector: aSelector ]
				ifTrue: [ 
					spec isRedrawable
						ifTrue: [ spec removeSubWidgets ]
						ifFalse: [ ^ spec instance ] ] ]
		ifFalse: [ spec := self computeSpecFrom: arrayToInterpret first selector: aSelector ].
	
	aSelector
		ifNil: [ aPresenter isSpecLayout ifTrue: [ spec selector: aPresenter selector ] ]
		ifNotNil: [ spec selector: aSelector ].
		
	^ nil