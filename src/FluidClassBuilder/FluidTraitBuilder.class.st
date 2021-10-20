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

{ #category : #API }
FluidTraitBuilder >> trait: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]

{ #category : #API }
FluidTraitBuilder >> traits: anArray [
	"To replace the uses: which will be deprecated"
	uses := anArray
]

{ #category : #deprecated }
FluidTraitBuilder >> uses: aTraitComposition [

	self deprecated:  'Please use #traits: instead' transformWith:  '`@receiver trait: `@arg' 
						-> '`@receiver traits: `@arg'.
						
	^ self traits: aTraitComposition
]
