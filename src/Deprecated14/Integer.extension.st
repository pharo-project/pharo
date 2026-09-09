Extension { #name : 'Integer' }

{ #category : '*Deprecated14' }
Integer >> metacelloIntegerLessThanSelf: anInteger [

	^anInteger < self
]

{ #category : '*Deprecated14' }
Integer >> metacelloSemanticIntegerLessThanSelf: anInteger [
    ^ anInteger < self
]

{ #category : '*Deprecated14' }
Integer >> metacelloSemanticStringLessThanSelf: aString [
    "string version components are always '<' integer component"

    ^ true
]

{ #category : '*Deprecated14' }
Integer >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticIntegerLessThanSelf: self
]

{ #category : '*Deprecated14' }
Integer >> metacelloStringLessThanSelf: aString [
	"string version components are always '<' integer component"
	
	^true
]

{ #category : '*Deprecated14' }
Integer >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloIntegerLessThanSelf: self
]
