Extension { #name : #Set }

{ #category : #'*Random-Core' }
Set >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand |

	self emptyCheck.
	rand := aGenerator nextInt: self size.
	self doWithIndex:[:each :ind |
		ind == rand ifTrue:[^each]].
	^ self errorEmptyCollection

]
