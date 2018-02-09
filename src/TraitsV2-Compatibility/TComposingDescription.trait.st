"
Kernel trait for ComposingDescription
"
Trait {
	#name : #TComposingDescription,
	#category : #'TraitsV2-Compatibility-Traits'
}

{ #category : #composition }
TComposingDescription >> + aTraitOrTraitComposition [
	"Use double dispatch to avoid having nested composition in cases where
	parenthesis are used, such as T1 + (T2 + T3)"
	
	^aTraitOrTraitComposition addOnTheLeft: self
]

{ #category : #composition }
TComposingDescription >> - anArrayOfSelectors [
	^TraitExclusion
		with: self
		exclusions: anArrayOfSelectors
]

{ #category : #composition }
TComposingDescription >> @ anArrayOfAssociations [ 
	^ TraitAlias with: self aliases: anArrayOfAssociations
]

{ #category : #private }
TComposingDescription >> addCompositionOnLeft: aTraitComposition [
	^ aTraitComposition add: self
]

{ #category : #private }
TComposingDescription >> addOnTheLeft: aTraitExpression [
	^TraitComposition with: aTraitExpression with: self
]

{ #category : #converting }
TComposingDescription >> asTraitComposition [
	^TraitComposition with: self
]
