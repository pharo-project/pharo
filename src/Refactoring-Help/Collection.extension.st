Extension { #name : #Collection }

{ #category : #'*Refactoring-Help' }
Collection >> collectAll: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect all the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | newCollection addAll: (aBlock value: each)].
	^ newCollection
]
