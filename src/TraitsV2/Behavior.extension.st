Extension { #name : #Behavior }

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
Behavior >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^ false
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
