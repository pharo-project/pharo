intervals
to: stop by: step do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 
step)."
	| nextValue |
	nextValue := self.
	step = 0 ifTrue: [self error: 'step must be non-zero'].
	step < 0
		ifTrue: [[stop <= nextValue]
				whileTrue: 
					[aBlock value: nextValue.
					nextValue := nextValue + step]]
		ifFalse: [[stop >= nextValue]
				whileTrue: 
					[aBlock value: nextValue.
					nextValue := nextValue + step]]