Class {
	#name : #FluidTraitBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassDefinition-Builder'
}

{ #category : #running }
FluidTraitBuilderTest >> setUp [

	builder := FluidTraitBuilder new 
	

]

{ #category : #tests }
FluidTraitBuilderTest >> testBuildSimplePoint2 [

	| trait |
	builder := Trait << #TPoint2
					slots: { #x . #y };
					package: 'FakedCore'.
	trait := builder build.
	
	self assert: trait name equals: #TPoint2.
	self assert: trait slots size equals: 2.

]

{ #category : #tests }
FluidTraitBuilderTest >> testTraitNoSlotsNoUse [

	| trait |
	builder := Trait << #TPoint3
		package: 'Fake'.
	trait := builder build.
	
	self assert: trait name equals: #TPoint3.
	self assert: trait package packageName equals: '_UnpackagedPackage'
	

]

{ #category : #tests }
FluidTraitBuilderTest >> testTraitUse [
	
	| trait |
	builder := Trait << #TPoint3
		uses: TViewModel;
		package: 'Fake'.
	trait := builder build.
	
	self assert: trait name equals: #TPoint3.
	self assert: trait traitComposition innerClass equals: TViewModel.
	self assert: trait package packageName equals: '_UnpackagedPackage'
]
