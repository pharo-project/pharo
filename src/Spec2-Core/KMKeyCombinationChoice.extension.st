Extension { #name : #KMKeyCombinationChoice }

{ #category : #'*Spec2-Core' }
KMKeyCombinationChoice >> spCurrentCharacter [
	^ shortcuts detect: [ :each | each spCurrentCharacter isNotNil ] ifNone: [ nil ]
]
