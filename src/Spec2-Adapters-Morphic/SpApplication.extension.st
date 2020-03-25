Extension { #name : #SpApplication }

{ #category : #'*Spec2-Adapters-Morphic' }
SpApplication >> styleSheet [
	"A stylesheet defines the application style. 
	 You can defins your own (but it don't, we will take the default)"

	^ self 
		propertyAt: #styleSheet 
		ifAbsent: [ SpStyle defaultStyleSheet ]
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpApplication >> styleSheet: aStyleClass [

	^ self 
		propertyAt: #styleSheet 
		put: aStyleClass
]
