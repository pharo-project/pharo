"
A trait to use as a mock in test classes like `FluidClassBuilderTest` and class `FluidTraitBuilderTest`
"
Trait {
	#name : #TViewModelMock,
	#classInstVars : [
		'a'
	],
	#category : #'FluidClassBuilder-Tests-Mocks'
}

{ #category : #accessing }
TViewModelMock classSide >> a [

	^ a
]

{ #category : #accessing }
TViewModelMock classSide >> a: anObject [

	a := anObject
]
