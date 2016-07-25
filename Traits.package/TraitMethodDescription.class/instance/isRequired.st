testing
isRequired
	self isEmpty ifTrue: [^ false].
	^ self locatedMethods allSatisfy: [:each | each isRequired]