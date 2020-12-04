Extension { #name : #StPharoApplication }

{ #category : #'*NewTools-Morphic' }
StPharoApplication >> useMorphic [

	self
		useBackend: #Morphic 
		with: StPharoMorphicConfiguration new
]
