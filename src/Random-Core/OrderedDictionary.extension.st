Extension { #name : #OrderedDictionary }

{ #category : #'*Random-Core' }
OrderedDictionary >> atRandom: aGenerator [
	| rand |
	self emptyCheck.
	rand := aGenerator nextInt: self size.
	^ self at: (self orderedKeys at: rand) 
]
