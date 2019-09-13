Extension { #name : #Morph }

{ #category : #'*Spec2-Adapters-Morphic' }
Morph >> spEnsureLayoutAndAddMorph: aMorph [

	aMorph layoutFrame
		ifNil: [ aMorph layoutFrame: LayoutFrame identity].
	self addMorph: aMorph 
]
