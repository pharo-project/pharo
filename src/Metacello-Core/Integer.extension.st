Extension { #name : 'Integer' }

{ #category : '*Metacello-Core' }
Integer >> metacelloIntegerLessThanSelf: anInteger [

	^anInteger < self
]

{ #category : '*Metacello-Core' }
Integer >> metacelloSemanticIntegerLessThanSelf: anInteger [
    ^ anInteger < self
]

{ #category : '*Metacello-Core' }
Integer >> metacelloSemanticStringLessThanSelf: aString [
    "string version components are always '<' integer component"

    ^ true
]

{ #category : '*Metacello-Core' }
Integer >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticIntegerLessThanSelf: self
]

{ #category : '*Metacello-Core' }
Integer >> metacelloStringLessThanSelf: aString [
	"string version components are always '<' integer component"
	
	^true
]

{ #category : '*Metacello-Core' }
Integer >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloIntegerLessThanSelf: self
]
