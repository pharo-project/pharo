private
uniqueInstance
	self = FFI_x86_64 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	^ super uniqueInstance