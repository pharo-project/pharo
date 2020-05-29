Extension { #name : #Set }

{ #category : #'*Random-Core' }
Set >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| index |

	self emptyCheck.
	index := aGenerator nextInt: array size.
	[ (array at: index) isNil ] whileTrue: [ index := index \\ array size + 1 ].
	^ array at: index.
]
