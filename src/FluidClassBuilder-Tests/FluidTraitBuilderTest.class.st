Class {
	#name : #FluidTraitBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassBuilder-Tests'
}

{ #category : #running }
FluidTraitBuilderTest >> removeFromSystem [

	self class environment at: #TMyClass 
		ifPresent: [:p | p removeFromSystem: false ]
		ifAbsent: [  ] .
	
]

{ #category : #running }
FluidTraitBuilderTest >> setUp [
	super setUp.
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
FluidTraitBuilderTest >> testInstallMinimalMockClass [

	| shiftClassBuilder installedClass |
	self removeFromSystem.
	[ 
	builder := self class compilerClass new
		           evaluate: 'Trait << #TMyClass
	uses: {};
	slots: {};
	tag: '''' ;
	package: ''MyPackage'''.
	shiftClassBuilder := builder shiftClassBuilder.
	ShiftClassInstaller new makeWithBuilder: shiftClassBuilder.
	
	installedClass := self class environment at: #TMyClass ifAbsent: [self fail].
	         
	self assert: installedClass name equals: #TMyClass.
	self assert: installedClass isFixed.
	self assert: installedClass slots isEmpty.
	self assert: installedClass traitComposition isEmpty ]
		ensure: [ self removeFromSystem  ]
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
		uses: TViewModelMock;
		package: 'Fake'.
	trait := builder build.
	
	self assert: trait name equals: #TPoint3.
	self assert: trait traitComposition innerClass equals: TViewModelMock.
	self assert: trait package packageName equals: '_UnpackagedPackage'
]
