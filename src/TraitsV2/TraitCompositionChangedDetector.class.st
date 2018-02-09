"
I can detect if there is a change in the Trait composition.

"
Class {
	#name : #TraitCompositionChangedDetector,
	#superclass : #ShAbstractChangeDetector,
	#category : #'TraitsV2-Class-Builder'
}

{ #category : #comparing }
TraitCompositionChangedDetector >> compareClass [

	(self enhancer traitCompositionOf: builder)
		~= (self enhancer traitCompositionOfClass: builder oldClass)
		ifTrue: [ ^ {(ShClassChanged new
				builder: builder;
				yourself)} ].

	(self enhancer classTraitCompositionOf: builder)
		~= (self enhancer classTraitCompositionOfClass: builder oldClass)
		ifTrue: [ ^ {(ShMetaclassChanged new
				builder: builder;
				yourself)} ].

	^ #()
]

{ #category : #accessing }
TraitCompositionChangedDetector >> enhancer [
	^ builder builderEnhancer
]
