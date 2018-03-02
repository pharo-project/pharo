Extension { #name : #Metaclass }

{ #category : #'*TraitsV2' }
Metaclass >> baseLocalMethods [
	^ self instanceSide methodDict
]

{ #category : #'*TraitsV2' }
Metaclass >> setTraitComposition: aTraitCompositionOrArray [
	
	^ self uses: aTraitCompositionOrArray slots: self slots.
]

{ #category : #'*TraitsV2' }
Metaclass >> uses: aTraitCompositionOrArray [
	
	^ self uses: aTraitCompositionOrArray slots: #().
]

{ #category : #'*TraitsV2' }
Metaclass >> uses: aTraitCompositionOrArray instanceVariableNames: instVarString [ 
	
	^ self uses: aTraitCompositionOrArray slots: instVarString asSlotCollection
]

{ #category : #'*TraitsV2' }
Metaclass >> uses: aTraitCompositionOrArray slots: slotArray [
	| theClass |
	theClass := self instanceSide.
	
	theClass := theClass classInstaller
		update: theClass
		to: [ :builder | 
			builder
				fillFor: theClass;
				classTraitComposition: aTraitCompositionOrArray asTraitComposition;				
				classSlots: slotArray ].
	^ theClass classSide
]
