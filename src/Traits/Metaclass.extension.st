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
	"This method will be deprecated better use trait."

	self trait: aTraitCompositionOrArray slots: #()
]

{ #category : '*Traits' }
Metaclass >> uses: aTraitCompositionOrArray instanceVariableNames: instVarString [
	"This method will be deprecated better use trait:slots: or trait:instanceVariableNames:"
	^ self uses: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : '*Traits' }
Metaclass >> uses: aTraitCompositionOrArray slots: slotArray [
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
