"
SUnit tests for header serialization in fuel
"
Class {
	#name : #FLHeaderSerializationTest,
	#superclass : #FLSerializationTest,
	#category : 'FuelTests'
}

{ #category : #tests }
FLHeaderSerializationTest >> testAdditionalObjects [

	| materialization |
	self serializer at: #test putAdditionalObject: 'test'.
	self serializer at: 42 putAdditionalObject: 68.
	
	self assertSerializationEqualityOf: 'foo'.

	self assert: (self materialization additionalObjectAt: #test) equals: 'test'.
	self assert: (self materialization additionalObjectAt: 42) equals: 68.
	
]

{ #category : #tests }
FLHeaderSerializationTest >> testJustMaterializeHeader [

	self serializer at: #test putAdditionalObject: 'test'.
	self serializer at: 42 putAdditionalObject: 68.
	
	self serialize: 'foo'.
	
	self assert: (self materializationHeader additionalObjectAt: #test) equals: 'test'.
	self assert: (self materializationHeader additionalObjectAt: 42) equals: 68.
	
]

{ #category : #tests }
FLHeaderSerializationTest >> testPostMaterializationActions [

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

]

{ #category : #tests }
FLHeaderSerializationTest >> testPreMaterializationActions [

	| aClass |
	aClass := self 
		newSubclassNamed: #FuelPreMaterializationTestClassToBeDeleted 
		of: Object
		instanceVariableNames: ''
		classVariableNames: 'TestClassVariable TestClassVariable2'.
	aClass class
		duringTestCompileSilently: 'postLoadMethod  TestClassVariable := 1';
		duringTestCompileSilently: 'postLoadMethod2  TestClassVariable := 2';
		duringTestCompileSilently: 'classVariable  ^TestClassVariable '.
	
	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) = 1.
	
	self serializer addPreMaterializationAction: [  (Smalltalk at: #FuelPreMaterializationTestClassToBeDeleted) perform: #postLoadMethod2  ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) = 1.
	self materialized.	
	self assert: (aClass perform: #classVariable) = 2.

]
