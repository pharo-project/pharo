traits
classesComposedWithMe
	self isTrait
		ifTrue: [ ^ self traitUsers flatCollect: [ :u | u classesComposedWithMe ] ]
		ifFalse: [ ^ {self} ]