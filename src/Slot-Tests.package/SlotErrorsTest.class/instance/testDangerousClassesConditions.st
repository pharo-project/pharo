tests
testDangerousClassesConditions

	| specialObjectsArrayItem |
	
	specialObjectsArrayItem := (Smalltalk specialObjectsArray select: [ :x | x isClass ]) anyOne name.

	self assert: (DangerousClassNotifier shouldNotBeRedefined: #ProtoObject).
	self assert: (DangerousClassNotifier shouldNotBeRedefined: specialObjectsArrayItem). 