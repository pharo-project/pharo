"
I can detect if there is a change in the Trait composition.
I will not rest longer!!! I should go to the modular trait library.
"
Class {
	#name : #ShTraitCompositionChangedDetector,
	#superclass : #ShAbstractClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #accessing }
ShTraitCompositionChangedDetector >> enhancer [
	^ builder builderEnhancer
]

{ #category : #initialization }
ShTraitCompositionChangedDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | self enhancer traitCompositionOf: e ].
	classAccessor := [ :e | e traitComposition ].
	comparer := [ :a :b | a asString = b asString ]
]
