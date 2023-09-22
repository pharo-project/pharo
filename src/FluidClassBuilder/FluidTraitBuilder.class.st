"
Similar to class `FluidClassBuilder` but for traits
"
Class {
	#name : 'FluidTraitBuilder',
	#superclass : 'FluidBuilder',
	#category : 'FluidClassBuilder-Base',
	#package : 'FluidClassBuilder',
	#tag : 'Base'
}

{ #category : 'building' }
FluidTraitBuilder >> fillShiftClassBuilder [

	super fillShiftClassBuilder.
	shiftClassBuilder
		beTrait;
		sharedVariables: #();
		sharedPools: ''.

	^ shiftClassBuilder
]
