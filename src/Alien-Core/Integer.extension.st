Extension { #name : #Integer }

{ #category : #'*Alien-Core' }
Integer >> | anInteger [
	^self bitOr: anInteger
]
