Extension { #name : 'Metaclass' }

{ #category : '*Traits' }
Metaclass >> baseLocalMethods [
	^ self instanceSide methodDict
]

{ #category : '*Traits' }
Metaclass >> initializeBasicMethods [

	"Nothing to do in the metaclass"
]

{ #category : '*Traits' }
Metaclass >> setTraitComposition: aTraitCompositionOrArray [

	^ self trait: aTraitCompositionOrArray slots: self slots
]

{ #category : '*Traits' }
Metaclass >> trait: aTraitCompositionOrArray [

	^ self trait: aTraitCompositionOrArray slots: #()
]

{ #category : '*Traits' }
Metaclass >> trait: aTraitCompositionOrArray instanceVariableNames: instVarString [
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv trait: `@arg1 instanceVariableNames: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2 asSlotCollection'.


	^ self trait: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : '*Traits' }
Metaclass >> trait: aTraitCompositionOrArray slots: slotArray [
	| theClass |
	theClass := self instanceSide.

	theClass := theClass classInstaller
		update: theClass
		to: [ :builder |
			builder
				classTraitComposition: aTraitCompositionOrArray asTraitComposition;
				classSlots: slotArray ].
	^ theClass classSide
]

{ #category : '*Traits' }
Metaclass >> uses: aTraitCompositionOrArray [

	self deprecated: 'Please use #trait: instead' transformWith: '`@receiver trait: `@arg' -> '`@receiver trait: `@arg'.

	^ self trait: aTraitCompositionOrArray
]

{ #category : '*Traits' }
Metaclass >> uses: aTraitCompositionOrArray instanceVariableNames: instVarString [
	
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv uses: `@arg1 instanceVariableNames: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2 asSlotCollection'.
	^ self trait: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : '*Traits' }
Metaclass >> uses: aTraitCompositionOrArray slots: slotArray [
	
	self
		deprecated: 'Use #trait:slots: instead.'
		transformWith: '`@rcv uses: `@arg1 slots: `@arg2' -> '`@rcv trait: `@arg1 slots: `@arg2'.
]
