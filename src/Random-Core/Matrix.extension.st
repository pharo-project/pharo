Extension { #name : #Matrix }

{ #category : #'*Random-Core' }
Matrix >> atRandom [
	"Return the contents of the receiver in a random order."
	^ contents atRandom

]

{ #category : #'*Random-Core' }
Matrix >> atRandom: aGenerator [
	^ contents atRandom: aGenerator
]
