Extension { #name : #Integer }

{ #category : #'*metacello-core' }
Integer >> metacelloIntegerLessThanSelf: anInteger [

	^anInteger < self
]

{ #category : #'*metacello-core' }
Integer >> metacelloSemanticIntegerLessThanSelf: anInteger [
    ^ anInteger < self
]

{ #category : #'*metacello-core' }
Integer >> metacelloSemanticStringLessThanSelf: aString [
    "string version components are always '<' integer component"

    ^ true
]

{ #category : #'*metacello-core' }
Integer >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticIntegerLessThanSelf: self
]

{ #category : #'*metacello-core' }
Integer >> metacelloStringLessThanSelf: aString [
	"string version components are always '<' integer component"
	
	^true
]

{ #category : #'*metacello-core' }
Integer >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloIntegerLessThanSelf: self
]
