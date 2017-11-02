Extension { #name : #KMKeyCombinationChoice }

{ #category : #'*Spec-Core' }
KMKeyCombinationChoice >> currentCharacter [
	| candidates |
	
	candidates := shortcuts 
		collect: [ :each | each currentCharacter ] 
		thenSelect: [ :each | each isNotNil ].
		
	^ candidates 
		ifEmpty: [ nil ]
		ifNotEmpty: [ candidates first ]
]
