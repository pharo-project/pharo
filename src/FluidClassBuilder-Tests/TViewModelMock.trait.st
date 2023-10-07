"
A trait to use as a mock in test classes like `FluidClassBuilderTest` and class `FluidTraitBuilderTest`
"
Trait {
	#name : 'TViewModelMock',
	#classInstVars : [
		'a'
	],
	#category : 'FluidClassBuilder-Tests-Mocks',
	#package : 'FluidClassBuilder-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
TViewModelMock classSide >> a [

	^ a
]

{ #category : 'accessing' }
TViewModelMock classSide >> a: anObject [

	a := anObject
]
