"
A trait to use as a mock in test classes like `FluidClassBuilderTest` and class `FluidTraitBuilderTest`
"
Trait {
	#name : 'TViewModelMock',
	#classInstVars : [
		'a'
	],
	#category : 'Shift-ClassBuilder-Tests',
	#package : 'Shift-ClassBuilder-Tests'
}

{ #category : 'accessing' }
TViewModelMock classSide >> a [

	^ a
]

{ #category : 'accessing' }
TViewModelMock classSide >> a: anObject [

	a := anObject
]
