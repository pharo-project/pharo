"
methods that make the user of this trait protocol-compatible with TraitTransformation.
When used in Trait and ClassTrait, this eliminates the need for TraitHolders.

"
Trait {
	#name : #TTransformationCompatibility,
	#category : #Traits-Kernel-Traits
}

{ #category : #enquiries }
TTransformationCompatibility >> aliasesForSelector: aSelector [ 
	^ OrderedCollection new

]

{ #category : #enquiries }
TTransformationCompatibility >> allAliasesDict [
	^IdentityDictionary new

]

{ #category : #enquiries }
TTransformationCompatibility >> changedSelectorsComparedTo: aTraitTransformation [
	| selectors otherSelectors changedSelectors aliases otherAliases |
	selectors := self allSelectors asIdentitySet.
	otherSelectors := aTraitTransformation allSelectors asIdentitySet.
	changedSelectors := IdentitySet withAll: (
		(selectors difference: otherSelectors) union: (otherSelectors difference: selectors)).
	aliases := self allAliasesDict.
	otherAliases := aTraitTransformation allAliasesDict.
	aliases keysAndValuesDo: [:key :value |
		(value ~~ (otherAliases at: key ifAbsent: [nil])) ifTrue: [changedSelectors add: key]].
	otherAliases keysAndValuesDo: [:key :value |
		(value ~~ (aliases at: key ifAbsent: [nil])) ifTrue: [changedSelectors add: key]].
	^ changedSelectors.

]

{ #category : #enquiries }
TTransformationCompatibility >> collectMethodsFor: aSelector into: methodDescription [
	(self includesSelector: aSelector) ifTrue: [ 
		methodDescription addLocatedMethod: 
			(self compiledMethodAt: aSelector)
		]
]

{ #category : #enquiries }
TTransformationCompatibility >> subject [
	"for compatibility with TraitTransformations"
	^ self

]

{ #category : #enquiries }
TTransformationCompatibility >> trait [
	"for compatibility with TraitTransformations"
	^ self

]
