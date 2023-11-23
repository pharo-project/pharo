Extension { #name : 'Behavior' }

{ #category : '*Traits' }
Behavior >> allTraits [

	<reflection: 'Class structural inspection - Traits'>
	^ #()
]

{ #category : '*Traits' }
Behavior >> hasTraitComposition [

	^ false
]

{ #category : '*Traits' }
Behavior >> isAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my or in another composition somewhere deeper in
	the tree of traits compositions."

	^ false
]

{ #category : '*Traits' }
Behavior >> isComposedBy: aTrait [
	"Answers if this object includes trait aTrait into its composition"
	aTrait isTrait ifFalse: [ ^false].
	^self hasTraitComposition
		and: [ self traitComposition includesTrait: aTrait ]
]

{ #category : '*Traits' }
Behavior >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^ false
]

{ #category : '*Traits' }
Behavior >> localMethodNamed: selector ifAbsent: aBlock [
	"Answer the locally defined method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary or it is not a local one, return the value of aBlock"

	^ self compiledMethodAt: selector ifAbsent: aBlock
]

{ #category : '*Traits' }
Behavior >> setTraitComposition: aComposition [

	^ self subclassResponsibility
]

{ #category : '*Traits' }
Behavior >> traitCompositionString [
	^ '{}'
]

{ #category : '*Traits' }
Behavior >> traits [
	<reflection: 'Class structural inspection - Traits'>
	^ #()
]

{ #category : '*Traits' }
Behavior >> users [
	^ #()
]
