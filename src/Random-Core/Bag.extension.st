Extension { #name : #Bag }

{ #category : #'*Random-Core' }
Bag >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	 only you use the generator. Causes an error if self has no elements."
	
	| rand index |
	self emptyCheck.
	rand := aGenerator nextInt: self size.
	"overwritten to use a faster enumeration"
	index := 0.
	self doWithOccurrences: [ :key :count | 
		index := index + count.
		rand <= index ifTrue: [ ^ key ] ]
]
