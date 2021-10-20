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

	shiftClassBuilder := ShiftClassBuilder new.
	shiftClassBuilder buildEnvironment: ShSmalltalkGlobalsEnvironment new. 
	shiftClassBuilder 
		name: nameToBuild;
		beTrait;
		sharedVariables: '';
		sharedPools: ''.
		
	slotsToBuild ifNotNil: [ shiftClassBuilder slots: slotsToBuild ].	
	uses ifNotNil: [ 
		shiftClassBuilder traitComposition: uses asTraitComposition.
		shiftClassBuilder classTraitComposition: uses asTraitComposition classComposition ].
	shiftClassBuilder classSlots: #().
	(tagToBuild isNil or: [ tagToBuild isEmpty ])
		ifTrue: [ shiftClassBuilder category: packageName ]
		ifFalse: [ tagToBuild isEmpty 
						ifFalse: [ shiftClassBuilder category: packageName, '-', tagToBuild asString ]
						ifTrue: [ shiftClassBuilder category: packageName ]
		].
	^ shiftClassBuilder
]
