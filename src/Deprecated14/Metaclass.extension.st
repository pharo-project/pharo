Extension { #name : 'Metaclass' }

{ #category : '*Deprecated14' }
Metaclass >> trait: aTraitCompositionOrArray instanceVariableNames: instVarString [
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv trait: `@arg1 instanceVariableNames: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2 asSlotCollection'.


	^ self trait: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : '*Deprecated14' }
Metaclass >> uses: aTraitCompositionOrArray [

	self deprecated: 'Please use #trait: instead' transformWith: '`@receiver trait: `@arg' -> '`@receiver trait: `@arg'.

	^ self trait: aTraitCompositionOrArray
]

{ #category : '*Deprecated14' }
Metaclass >> uses: aTraitCompositionOrArray instanceVariableNames: instVarString [
	
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv uses: `@arg1 instanceVariableNames: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2 asSlotCollection'.
	^ self trait: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : '*Deprecated14' }
Metaclass >> uses: aTraitCompositionOrArray slots: slotArray [
	
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv uses: `@arg1 slots: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2'.
]
