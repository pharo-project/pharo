"
This class contains tests for `FluidClassSideTraitBuilder`
"
Class {
	#name : 'FluidClassSideTraitBuilderTest',
	#superclass : 'FluidClassBuilderAbstractTest',
	#category : 'FluidClassBuilder-Tests-Base',
	#package : 'FluidClassBuilder-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FluidClassSideTraitBuilderTest >> setUp [

	super setUp.
	builder := ShiftClassSideBuilder new
		           beTrait;
		           name: TViewModelMock name
]

{ #category : 'initialization' }
FluidClassSideTraitBuilderTest >> testSlots [

	| trait |
	builder := (Trait << TViewModelMock classTrait)
		           slots: { #x. #y };
		           package: self packageNameForTest.
	trait := builder build.

	self assert: trait name equals: #TViewModelMock.
	self assert: trait class slots size equals: 2
]
