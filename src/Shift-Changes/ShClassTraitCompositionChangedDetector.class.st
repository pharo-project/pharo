"
I can detect if there is a change in the class Trait composition.
I will not rest longer!!! I should go to the modular trait library.
"
Class {
	#name : #ShClassTraitCompositionChangedDetector,
	#superclass : #ShTraitCompositionChangedDetector,
	#category : 'Shift-Changes'
}

{ #category : #initialization }
ShClassTraitCompositionChangedDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | self enhancer classTraitCompositionOf: e ].
	classAccessor := [ :e | e class traitComposition ]
	
]
