private
process: anExample withArguments: aBlock
	| currentResult arguments |
	currentResult := self newResultFor: anExample.
	arguments := nil.
	anExample hasValidArguments 
		ifFalse: [ ^ self onArgumentError value: self value: anExample value: arguments ].
	[ arguments := aBlock value ]
		on: Error
		do: [ :exception | ^ self onDependencyError value: self value: anExample value: exception ].
	self do: [ currentResult returnValue: (self primitiveProcessExample: anExample withArguments: arguments) ]
		on: anExample signalableExceptions
		do: [ :exception | currentResult expectedError: exception ].
	processed add: currentResult.
	self do: [ self processAfter: anExample ]
		on: anExample signalableExceptions
		do: [ :exception | currentResult expectedError: exception ].
	anExample == self example ifTrue: [ result := currentResult ].
	anExample exceptions signaledBy: currentResult expectedError.
	^ currentResult returnValue