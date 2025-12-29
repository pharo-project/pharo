Extension { #name : 'Integer' }

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
