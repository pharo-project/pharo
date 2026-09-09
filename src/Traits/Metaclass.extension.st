Extension { #name : 'Metaclass' }

{ #category : '*Traits' }
Metaclass >> baseLocalMethods [
	^ self instanceSide methodDict
]

{ #category : '*Traits' }
Metaclass >> basicTraitComposition [
	"Compatibility method to match TraitedMetaclass."

	^ self traitComposition
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
