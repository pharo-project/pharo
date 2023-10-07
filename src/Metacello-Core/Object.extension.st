Extension { #name : 'Object' }

{ #category : '*Metacello-Core' }
Object >> metacelloIntegerLessThanSelf: anInteger [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : '*Metacello-Core' }
Object >> metacelloSemanticIntegerLessThanSelf: anInteger [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : '*Metacello-Core' }
Object >> metacelloSemanticStringLessThanSelf: anInteger [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : '*Metacello-Core' }
Object >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : '*Metacello-Core' }
Object >> metacelloStringLessThanSelf: anInteger [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]

{ #category : '*Metacello-Core' }
Object >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^self error: 'Invalid Metacello verson component - should be String or Integer.'
]
