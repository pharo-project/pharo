private
location: aLocationString
	self = IceMetacelloRepositoryType 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	
	^ self basicNew
		initializeLocation: aLocationString;
		yourself