Extension { #name : #Collection }

{ #category : #'*Deprecated90' }
Collection >> detectSum: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Return the sum of the answers."
	| sum |
	self deprecated: 'Use #sum: instead'.
	sum := 0.
	self do: [:each | 
		sum := (aBlock value: each) + sum].  
	^ sum
]
