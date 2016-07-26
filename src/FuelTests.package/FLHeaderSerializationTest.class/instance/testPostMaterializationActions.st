tests
testPostMaterializationActions

	| aClass |
	aClass := self 
		newSubclassNamed: #FuelPostMaterializationTestClassToBeDeleted 
		of: Object
		instanceVariableNames: ''
		classVariableNames: 'TestClassVariable TestClassVariable2'.
	aClass class
		duringTestCompileSilently: 'postLoadMethod  TestClassVariable := 1';
		duringTestCompileSilently: 'postLoadMethod2  TestClassVariable := 2';
		duringTestCompileSilently: 'classVariable  ^TestClassVariable '.
	
	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) = 1.
	
	self serializer addPostMaterializationAction: [  (Smalltalk at: #FuelPostMaterializationTestClassToBeDeleted) perform: #postLoadMethod2  ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) = 1.
	self materialized.	
	self assert: (aClass perform: #classVariable) = 2.
