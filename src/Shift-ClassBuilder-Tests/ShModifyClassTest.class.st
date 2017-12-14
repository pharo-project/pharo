Class {
	#name : #ShModifyClassTest,
	#superclass : #ShAbstractClassBuilderTest,
	#category : 'Shift-ClassBuilder-Tests'
}

{ #category : #tests }
ShModifyClassTest >> testEmptyClass [
	builder name: #ShCBEmptyClass.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #()

]

{ #category : #tests }
ShModifyClassTest >> testEmptyClassWithMethods [
	builder name: #ShCBEmptyClassWithMethods.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(aMethod anotherMethod:)

]
