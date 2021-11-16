"
Similar to class `FluidClassBuilder` but for traits
"
Class {
	#name : #FluidTraitBuilder,
	#superclass : #FluidBuilder,
	#category : #'FluidClassBuilder-Base'
}

{ #category : #accessing }
FluidTraitBuilder >> fillShiftClassBuilder [
	
	super fillShiftClassBuilder.
	shiftClassBuilder 
		beTrait;
		sharedVariables: #();
		sharedPools: ''.
		
	^ shiftClassBuilder
]
