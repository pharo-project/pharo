instance creation
basicNew

	self == Integer ifTrue: [
		^ self error: 'Integer is an abstract class.  Make a concrete subclass.'].
	^ super basicNew