Extension { #name : #Collection }

{ #category : #'*Random-Core' }
Collection >> atRandom [
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ self atRandom: SharedRandom globalGenerator.

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"
]

{ #category : #'*Random-Core' }
Collection >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand index |

	self emptyCheck.
	rand := aGenerator nextInt: self size.
	index := 1.
	self do: [:each |
		index = rand ifTrue: [^each].
		index := index + 1].
	^ self errorEmptyCollection

]
