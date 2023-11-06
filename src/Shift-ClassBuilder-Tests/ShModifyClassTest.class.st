Class {
	#name : 'ShModifyClassTest',
	#superclass : 'ShAbstractClassBuilderTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
ShModifyClassTest >> testEmptyClass [

	builder name: #ShCBEmptyClass.
	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(  ).
	self assert: result hasComment
]

{ #category : 'tests' }
ShModifyClassTest >> testEmptyClassWithMethods [

	builder name: #ShCBEmptyClassWithMethods.
	result := builder build.

	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #( aMethod anotherMethod: )
]
