Extension { #name : #Behavior }

{ #category : #'*TraitsV2' }
Behavior >> allTraits [
	^ #()
]

{ #category : #'*TraitsV2' }
Behavior >> hasTraitComposition [
	
	^ false 
]

{ #category : #'*TraitsV2' }
Behavior >> isAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my or in another composition somewhere deeper in 
	the tree of traits compositions."

	^ false
]

{ #category : #'*TraitsV2' }
Behavior >> isComposedBy: aTrait [
	"Answers if this object includes trait aTrait into its composition"
	aTrait isTrait ifFalse: [ ^false].
	^self hasTraitComposition 
		ifTrue: [ self traitComposition includesTrait: aTrait ]
		ifFalse: [ false ]
]

{ #category : #'*TraitsV2' }
Behavior >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^ false
]

{ #category : #'*TraitsV2' }
Behavior >> localMethodNamed: selector ifAbsent: aBlock [
	"Answer the locally defined method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary or it is not a local one, return the value of aBlock"

	^ self compiledMethodAt: selector ifAbsent: aBlock.
]

{ #category : #'*TraitsV2' }
Behavior >> localMethods [
	"returns the methods of classes excluding the ones of the traits that the class uses" 
	 
	^ self methods
]

{ #category : #'*TraitsV2' }
Behavior >> localSelectors [
	
	^ methodDict keys
]

{ #category : #'*TraitsV2' }
Behavior >> setTraitComposition: aComposition [

	^ self subclassResponsibility 
]

{ #category : #'*TraitsV2' }
Behavior >> traitCompositionString [
	^ '{}'
]

{ #category : #'*TraitsV2' }
Behavior >> traits [
	^ #()
]

{ #category : #'*TraitsV2' }
Behavior >> users [
	^ #()
]
