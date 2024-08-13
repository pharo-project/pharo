"
This class contains tests for `FluidClassSideClassBuilder`
"
Class {
	#name : 'ShiftClassSideClassBuilderTest',
	#superclass : 'ShiftClassBuilderAbstractTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ShiftClassSideClassBuilderTest >> setUp [

	super setUp.
	"as we can see the superclass and name are the ones of the class (not the metaclass)"
	builder := ShiftClassSideBuilder new
		           superclass: Object;
		           name: #Point33;
		           package: 'FakedPackage'
]

{ #category : 'tests' }
ShiftClassSideClassBuilderTest >> testBuilderClassName [
	"Yes a class side builder expects the instance name."

	self assert: builder name equals: #'Point33'
]

{ #category : 'tests' }
ShiftClassSideClassBuilderTest >> testBuilderSuperclass [

	self assert: builder superclass equals: Object
]

{ #category : 'tests' }
ShiftClassSideClassBuilderTest >> testBuilderWithSlots [

	builder slots: { #metaX . #metaY }.
	"the builder is class side builder so we are setting the class level"

	self assert: builder classSlots equals: {#metaX => InstanceVariableSlot. #metaY => InstanceVariableSlot}.
	self assert: builder slots equals: #()
]

{ #category : 'tests' }
ShiftClassSideClassBuilderTest >> testBuilderWithTraits [

	builder traits: { TViewModelMock classTrait }.
	"the builder is class side builder so we are setting the class level"

	self assertEmpty: builder traitComposition.
	self assert: builder classTraitComposition name equals: TViewModelMock classTrait name
]
