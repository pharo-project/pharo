Extension { #name : #Number }

{ #category : #'*Collections-Sequenceable' }
Number >> to: stop [
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by 1."

	^Interval from: self to: stop by: 1
]

{ #category : #'*Collections-Sequenceable' }
Number >> to: stop by: step [
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by step."

	^Interval from: self to: stop by: step
]

{ #category : #'*Collections-Sequenceable' }
Number >> to: stop by: step do: aBlock [
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
]

{ #category : #'*Collections-Sequenceable' }
Number >> to: stop do: aBlock [
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue := self.
	[nextValue <= stop]
		whileTrue:
			[aBlock value: nextValue.
			nextValue := nextValue + 1]
]
