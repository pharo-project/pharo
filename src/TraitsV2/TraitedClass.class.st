Class {
	#name : #TraitedClass,
	#superclass : #Object,
	#classInstVars : [
		'users'
	],
	#category : #TraitsV2
}

{ #category : #'accessing method dictionary' }
TraitedClass class >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: aCategory notifying: requestor [

	| r |
	r := super
		addAndClassifySelector: selector
		withMethod: compiledMethod
		inProtocol: aCategory
		notifying: requestor.

	self propagateChangeOf: selector.

	^ r
	
]

{ #category : #'accessing method dictionary' }
TraitedClass class >> addSelector: selector withMethod: compiledMethod notifying: requestor [
	| r |
	r := super addSelector: selector withMethod: compiledMethod notifying: requestor.
	self propagateChangeOf: selector.
	
	^ r
]

{ #category : #users }
TraitedClass class >> addUser: aClass [

	self basicUsers add: aClass.
]

{ #category : #users }
TraitedClass class >> basicUsers [
	^ users ifNil: [ users := WeakSet new ]
]

{ #category : #accessing }
TraitedClass class >> classTrait [
	^ self class
]

{ #category : #accessing }
TraitedClass class >> isTrait [
	^ true
]

{ #category : #changes }
TraitedClass class >> propagateChangeOf: aSelector [
	| change |

	change := TraitChange new
		updatedSelectors: {aSelector};
		yourself.
		
	self basicUsers do: [ :e | change applyOn: e ]
]

{ #category : #users }
TraitedClass class >> removeUser: aClass [

	self basicUsers remove: aClass ifAbsent: [ ].
]

{ #category : #accessing }
TraitedClass class >> traitComposition [
	
	^ TaEmptyComposition new
]

{ #category : #users }
TraitedClass class >> traitUsers [
	^ #()
]

{ #category : #'accessing method dictionary' }
TraitedClass >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor [
	| r |
	
	self localMethodDict at: selector put: compiledMethod.
	
	r := super
		addAndClassifySelector: selector
		withMethod: compiledMethod
		inProtocol: category
		notifying: requestor.

	TraitChange addSelector: selector on: self.
	^ r
	
]

{ #category : #'accessing method dictionary' }
TraitedClass >> addSelector: selector withMethod: compiledMethod notifying: requestor [
	| r |
	self localMethodDict at: selector put: compiledMethod.

	r := super addSelector: selector withMethod: compiledMethod notifying: requestor.
	TraitChange addSelector: selector on: self..
	^ r
]

{ #category : #adding }
TraitedClass >> addSlot: aSlot [

	^self classInstaller update: self to: [ :builder |
		builder
			fillFor: self;
			slots: (self slots copyWith: aSlot)].
]

{ #category : #querying }
TraitedClass >> allTraits [
	^ self traitComposition allTraits
]

{ #category : #initialization }
TraitedClass >> doRebuildMethodDictionary [
	| selectors removedSelectors removeFromOrganization modified|

	modified := false.
	self methodDict valuesDo: [ :m | (m propertyAt: #traitSource) ifNil: [ self localMethodDict at: m selector put: m ]].

	selectors := self traitComposition selectors reject: [ :e | (self localMethodDict includesKey: e) ].
	selectors do: [ :e | modified := modified | (self traitComposition installSelector: e into: self replacing: false)].

	removedSelectors := self methodDict keys reject: [ :aSelector | (selectors includes: aSelector) or: [ self localMethodDict includesKey: aSelector] ].
	modified := modified | (removedSelectors isNotEmpty).
	removedSelectors do: [ :aSelector | self methodDict removeKey: aSelector ].

	removeFromOrganization := self organization allMethodSelectors reject: [:e | self methodDict includesKey: e ].
	removeFromOrganization do: [ :aSelector | self organization removeElement: aSelector ].

	^ modified
]

{ #category : #testing }
TraitedClass >> hasTraitComposition [
	
	^ self traitComposition isEmpty not
]

{ #category : #testing }
TraitedClass >> includesLocalSelector: aSymbol [
	^ self localMethodDict includesKey: aSymbol
]

{ #category : #testing }
TraitedClass >> isAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my or in another composition somewhere deeper in 
	the tree of traits compositions."

	^ self traitComposition isAliasSelector: aSymbol
]

{ #category : #testing }
TraitedClass >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^ self traitComposition isLocalAliasSelector: aSymbol
]

{ #category : #testing }
TraitedClass >> isLocalMethodsProtocol: aProtocol [
	aProtocol methods ifEmpty: [ ^true ].
	
	^aProtocol methods anySatisfy: [ :each | self isLocalSelector: each]
]

{ #category : #testing }
TraitedClass >> isLocalSelector: aSelector [
	
	^ self localMethodDict includesKey: aSelector
]

{ #category : #accessing }
TraitedClass >> localMethodDict [
	^ self class baseLocalMethods
]

{ #category : #accessing }
TraitedClass >> localMethodDict: aMethodDictionary [
	^ self class baseLocalMethods: aMethodDictionary 
]

{ #category : #accessing }
TraitedClass >> localMethods [
	"returns the methods of classes excluding the ones of the traits that the class uses" 
	 
	^ self localMethodDict values
]

{ #category : #'accessing method dictionary' }
TraitedClass >> localSelectors [
	
	^ self localMethodDict keys
]

{ #category : #testing }
TraitedClass >> originMethod: aMethod [
	(self localMethodDict includesKey: aMethod selector)
		ifTrue: [ ^ aMethod ].

	(aMethod hasProperty: #traitSource)
		ifTrue: [ |newSelector|
			newSelector := self traitComposition originSelectorOf: aMethod selector.
			^ (aMethod propertyAt: #traitSource) compiledMethodAt: newSelector ifAbsent: [aMethod] ].

	^ (self traitComposition traitDefining: aMethod selector ifNone: [ self ])
		compiledMethodAt: aMethod selector ifAbsent: [ ^ aMethod ]		

]

{ #category : #testing }
TraitedClass >> originOfMethod: aMethod [

	(aMethod hasProperty: #traitSource) 
		ifTrue: [ ^ (aMethod propertyAt: #traitSource) innerClass ]. 

	(self localMethodDict includesKey: aMethod selector)
		ifTrue: [ ^ self ].

	^ (self traitComposition traitDefining: aMethod selector ifNone: [ ^ self ]) innerClass
]

{ #category : #initialization }
TraitedClass >> rebuildMethodDictionary [

	"Useful to be rewritten in Traits"
	^ self doRebuildMethodDictionary.

]

{ #category : #categories }
TraitedClass >> recategorizeSelector: selector from: oldCategory to: newCategory [
	| original |
	
	original := self organization categoryOfElement: selector ifAbsent: [ ^ self ].

	"If it is nil is because it is a removal. It will removed when the method is removed."
	newCategory ifNil: [ ^ self ].

	original = oldCategory
		ifTrue: [ self organization classify: selector under: newCategory suppressIfDefault: true ].

	(self traitComposition reverseAlias: selector) do: [ :e | 
		self recategorizeSelector: e from: oldCategory to: newCategory. 
		self notifyOfRecategorizedSelector: e from: oldCategory to: newCategory ].
	
	self organization removeEmptyCategories
]

{ #category : #'trait-composition' }
TraitedClass >> removeFromComposition: aTrait [

	self setTraitComposition: (self traitComposition copyWithoutTrait: aTrait asTraitComposition)
]

{ #category : #removing }
TraitedClass >> removeFromSystem: logged [

	self traitComposition removeUser: self.
	self class traitComposition removeUser: self class.

	TraitedClass removeUser: self class.

	super removeFromSystem: logged.
]

{ #category : #removing }
TraitedClass >> removeSelector: aSelector [

	super removeSelector: aSelector.
	self localMethodDict removeKey: aSelector ifAbsent: [  ].

	TraitChange removeSelector: aSelector on: self.
]

{ #category : #removing }
TraitedClass >> removeSlot: aSlot [ 
	
	(self classLayout slots includes: aSlot)
		ifFalse: [self error: aSlot name , ' is not one of my slots'].
		
	^self classInstaller update: self to: [ :builder |
		builder
			fillFor: self;
			slots: (self classLayout slots copyWithout: aSlot)]
	
]

{ #category : #initialization }
TraitedClass >> slots [
	^ super slots reject:[ :e  | self traitComposition slots includes:e ]
]

{ #category : #'accessing tags' }
TraitedClass >> tagsForMethods [
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	For now we could only implemented it on top of Protocol. 
	It supposed to not include any method tags inherited from Traits 
	which is opposite to current Protocol implementation.
	And extension protocol is not treated as tag"
	| allProtocols tags |
	allProtocols := self organization protocols 
		reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].
		
	tags := OrderedCollection new.
	allProtocols do: [ :each | 
		(self isLocalMethodsProtocol: each) ifTrue: [ tags add: each name ]].

	^tags
]

{ #category : #accessing }
TraitedClass >> traitComposition [

	^ self class baseComposition
]

{ #category : #accessing }
TraitedClass >> traitComposition: anObject [
	
	self class baseComposition: anObject
]

{ #category : #accessing }
TraitedClass >> traitCompositionString [
	^ self traitComposition asString
]

{ #category : #accessing }
TraitedClass >> traitUsers [
	
	^ #()
]

{ #category : #accessing }
TraitedClass >> traits [
	^ self traitComposition traits.
]
