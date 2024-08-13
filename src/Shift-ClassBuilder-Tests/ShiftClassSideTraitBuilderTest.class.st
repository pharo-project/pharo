"
This class contains tests for `FluidClassSideTraitBuilder`
"
Class {
	#name : 'ShiftClassSideTraitBuilderTest',
	#superclass : 'ShiftClassBuilderAbstractTest',
	#category : 'Shift-ClassBuilder-Tests-Base',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ShiftClassSideTraitBuilderTest >> setUp [

	super setUp.
	builder := ShiftClassSideBuilder new
		           beTrait;
		           name: TViewModelMock name
]

{ #category : 'initialization' }
ShiftClassSideTraitBuilderTest >> testSlots [

	| trait |
	builder := (Trait << TViewModelMock classTrait)
		           slots: { #x. #y };
		           package: self packageNameForTest.
	trait := builder build.

	self assert: trait name equals: #TViewModelMock.
	self assert: trait class slots size equals: 2
]
