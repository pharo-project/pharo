Extension { #name : #GLMAction }

{ #category : #'*glamour-morphic-renderer' }
GLMAction >> installKeyCombinationForPresentation: aPresentation onMorph: aMorph [
	aMorph 
		bindKeyCombination: self keymap		
		toAction: [ self actOn: aPresentation ].
	
]

{ #category : #'*glamour-morphic-renderer' }
GLMAction >> morphicActOn: aPresentation [

	^ self actOn: aPresentation
]
