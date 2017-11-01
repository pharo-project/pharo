Extension { #name : #Object }

{ #category : #'*metacello-core' }
Object >> metacelloIntegerLessThanSelf: anInteger [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : #'*metacello-core' }
Object >> metacelloSemanticIntegerLessThanSelf: anInteger [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : #'*metacello-core' }
Object >> metacelloSemanticStringLessThanSelf: anInteger [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : #'*metacello-core' }
Object >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : #'*metacello-core' }
Object >> metacelloStringLessThanSelf: anInteger [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : #'*metacello-core' }
Object >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]
