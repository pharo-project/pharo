Class {
	#name : 'FLFullHeaderSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'tests' }
FLFullHeaderSerializationTest >> testAdditionalObjects [

	self serializer
		at: #test putAdditionalObject: 'test';
		at: 42 putAdditionalObject: 68.
	
	self assertSerializationEqualityOf: 'foo'.

	self assert: (self materializedObjects header additionalObjectAt: #test) equals: 'test'.
	self assert: (self materializedObjects header additionalObjectAt: 42) equals: 68
]

{ #category : 'tests' }
FLFullHeaderSerializationTest >> testJustMaterializeHeader [
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
FLFullHeaderSerializationTest >> testPostMaterializationActions [

	| aClass |
	OCCompilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	aClass := self classFactory make: [ :aBuilder | aBuilder sharedVariables: #(TestClassVariable TestClassVariable2) ].
	self classFactory
		silentlyCompile: 'postLoadMethod TestClassVariable := 1' in: aClass class;
		silentlyCompile: 'postLoadMethod2 TestClassVariable := 2' in: aClass class;
		silentlyCompile: 'classVariable ^ TestClassVariable ' in: aClass class.

	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) equals: 1.
	self serializer
		at: #className putAdditionalObject: aClass name;
		fullySerializeBehavior: aClass;
		addPostMaterializationAction: [ :materializedObjects :environment |
			(environment at: (materializedObjects additionalObjectAt: #className)) perform: #postLoadMethod2 ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) equals: 1.
	self materialized.
	self assert: (aClass perform: #classVariable) equals: 2
]

{ #category : 'tests' }
FLFullHeaderSerializationTest >> testPostMaterializationActionsMustBeClean [
	self
		should: [ self serializer addPostMaterializationAction: [ self ] ]
		raise: Error
		description: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean'
]

{ #category : 'tests' }
FLFullHeaderSerializationTest >> testPreMaterializationActions [

	| aClass |
	OCCompilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	aClass := self classFactory make: [ :aBuilder | aBuilder sharedVariables: #(TestClassVariable TestClassVariable2) ].
	self classFactory
		silentlyCompile: 'postLoadMethod TestClassVariable := 1' in: aClass class;
		silentlyCompile: 'postLoadMethod2 TestClassVariable := 2' in: aClass class;
		silentlyCompile: 'classVariable ^TestClassVariable' in: aClass class.

	aClass perform: #postLoadMethod.
	self assert: (aClass perform: #classVariable) equals: 1.
	self serializer
		at: #className putAdditionalObject: aClass name;
		addPreMaterializationAction: [ :header :environment | (environment at: (header additionalObjectAt: #className)) perform: #postLoadMethod2 ].
	self serialize: aClass.
	self assert: (aClass perform: #classVariable) equals: 1.
	self materialized.
	self assert: (aClass perform: #classVariable) equals: 2
]

{ #category : 'tests' }
FLFullHeaderSerializationTest >> testPreMaterializationActionsMustBeClean [
	self
		should: [ self serializer addPreMaterializationAction: [ self ] ]
		raise: Error
		description: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean'
]
