Extension { #name : #KMKeyCombinationChoice }

{ #category : #'*Spec2-Core' }
KMKeyCombinationChoice >> spCurrentCharacter [

	shortcuts do: [ :each | 
		each spCurrentCharacter ifNotNil: [ :char | ^ char ] ].
	^ nil
]

{ #category : #'*Spec2-Core' }
KMKeyCombinationChoice >> spModifier [

	shortcuts do: [ :each | 
		each spModifier ifNotNil: [ :mod | ^ mod ] ].
	^ nil
]
