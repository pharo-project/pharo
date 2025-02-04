"
SUnit tests for header serialization in fuel
"
Class {
	#name : 'FLHeaderSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLHeaderSerializationTest >> testAdditionalObjects [

	self serializer
		at: #test putAdditionalObject: 'test';
		at: 42 putAdditionalObject: 68.
	
	self assertSerializationEqualityOf: 'foo'.

	self assert: (self materializedObjects additionalObjectAt: #test) equals: 'test'.
	self assert: (self materializedObjects additionalObjectAt: 42) equals: 68.
]

{ #category : 'tests' }
FLHeaderSerializationTest >> testJustMaterializeHeader [
	| header |
	self serializer
		at: #test putAdditionalObject: 'test';
		at: 42 putAdditionalObject: 68.
	
	self serialize: 'foo'.
	
	header := self materializer materializeHeader.
	self assert: (header additionalObjectAt: #test) equals: 'test'.
	self assert: (header additionalObjectAt: 42) equals: 68
]

{ #category : 'tests' }
FLHeaderSerializationTest >> testPostMaterializationActions [

	| aClass |
	Smalltalk compiler compilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	aClass := self classFactory make: [ :aBuilder | aBuilder sharedVariables: #(TestClassVariable TestClassVariable2) ].
	self classFactory
		silentlyCompile: 'postLoadMethod TestClassVariable := 1' in: aClass class;
		silentlyCompile: 'postLoadMethod2 TestClassVariable := 2' in: aClass class;
		silentlyCompile: 'classVariable ^ TestClassVariable ' in: aClass class.

	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) equals: 1.
	self serializer
		at: #class putAdditionalObject: aClass;
		addPostMaterializationAction: [ :header | "Can't use `self environmentOfTest` or `aClass name` here because the closure
			must be clean."
			(header additionalObjectAt: #class) perform: #postLoadMethod2 ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) equals: 1.
	self materialized.
	self assert: (aClass perform: #classVariable) equals: 2
]

{ #category : 'tests' }
FLHeaderSerializationTest >> testPostMaterializationActionsMustBeClean [
	self
		should: [ self serializer addPostMaterializationAction: [ self ] ]
		raise: Error
		description: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean'
]

{ #category : 'tests' }
FLHeaderSerializationTest >> testPreMaterializationActions [

	| aClass |
	Smalltalk compiler compilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	aClass := self classFactory make: [ :aBuilder | aBuilder sharedVariables: #(TestClassVariable TestClassVariable2) ].
	self classFactory
		silentlyCompile: 'postLoadMethod TestClassVariable := 1' in: aClass class;
		silentlyCompile: 'postLoadMethod2 TestClassVariable := 2' in: aClass class;
		silentlyCompile: 'classVariable ^TestClassVariable' in: aClass class.

	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) equals: 1.
	self serializer
		at: #class putAdditionalObject: aClass;
		addPreMaterializationAction: [ :header | "Can't use `self environmentOfTest` or `aClass name` here because the closure
			must be clean."
			(header additionalObjectAt: #class) perform: #postLoadMethod2 ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) equals: 1.
	self materialized.
	self assert: (aClass perform: #classVariable) equals: 2
]

{ #category : 'tests' }
FLHeaderSerializationTest >> testPreMaterializationActionsMustBeClean [
	self
		should: [ self serializer addPreMaterializationAction: [ self ] ]
		raise: Error
		description: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean'
]
