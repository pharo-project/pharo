tests
during: aBlock rename: aClass as: anotherClass

	[ Smalltalk at: aClass name put: anotherClass.
	^aBlock value ] ensure: [ 
	Smalltalk at: aClass name put: aClass ].