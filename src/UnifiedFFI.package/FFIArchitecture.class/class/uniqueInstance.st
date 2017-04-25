private
uniqueInstance
	self = FFIArchitecture 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]