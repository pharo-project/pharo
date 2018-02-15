Class {
	#name : #TraitedMetaclass,
	#superclass : #Metaclass,
	#instVars : [
		'localMethods',
		'composition',
		'baseLocalMethods',
		'baseComposition'
	],
	#category : #TraitsV2
}

{ #category : #accessing }
TraitedMetaclass class >> traitedClassTrait [
	^ (TaCompositionElement for: TraitedClass)
]

{ #category : #'accessing method dictionary' }
TraitedMetaclass >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor [
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
TraitedMetaclass >> addSelector: selector withMethod: compiledMethod notifying: requestor [
	| r |
	self localMethodDict at: selector put: compiledMethod.
	
	r := super addSelector: selector withMethod: compiledMethod notifying: requestor.
	TraitChange addSelector: selector on: self.
	^ r
]

{ #category : #querying }
TraitedMetaclass >> allTraits [
	
	^ self traitComposition allTraits
]

{ #category : #accessing }
TraitedMetaclass >> baseComposition [
	^ baseComposition
]

{ #category : #accessing }
TraitedMetaclass >> baseComposition: anObject [
	baseComposition := anObject
]

{ #category : #accessing }
TraitedMetaclass >> baseLocalMethods [
	^ baseLocalMethods
]

{ #category : #accessing }
TraitedMetaclass >> baseLocalMethods: anObject [
	baseLocalMethods := anObject
]

{ #category : #'fileIn/Out' }
TraitedMetaclass >> definition [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: 
		[:strm |
		strm print: self.
		self hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: self traitComposition ].
		
		(self usesSpecialSlot or: [ Slot showSlotClassDefinition ])
			ifFalse: [  
						strm
							crtab;
							nextPutAll: 'instanceVariableNames: ';
							store: self instanceVariablesString]
			ifTrue: [ 
						strm
							crtab;
							nextPutAll: 'slots: ';
							nextPutAll: self slotDefinitionString]]
]

{ #category : #initialization }
TraitedMetaclass >> emptyMethodDictionary [
	^ MethodDictionary new: 64.
]

{ #category : #testing }
TraitedMetaclass >> hasTraitComposition [
	
	^ self traitComposition isEmpty not
]

{ #category : #testing }
TraitedMetaclass >> includesLocalSelector: aSymbol [
	^ localMethods includesKey: aSymbol
]

{ #category : #initialization }
TraitedMetaclass >> initialize [
	super initialize.
	localMethods := self emptyMethodDictionary.
	composition := TaEmptyComposition new.
	
	baseComposition := TaEmptyComposition new.
	baseLocalMethods := self emptyMethodDictionary.
]

{ #category : #initialization }
TraitedMetaclass >> initializeBasicMethods [
	| selectors |
	selectors := self class traitedClassTrait selectors reject: [ :e | self isRejectedMethod: e ].
	selectors do: [ :e | self class traitedClassTrait copyMethod: e into: self replacing: true ]
]

{ #category : #'initialize-release' }
TraitedMetaclass >> instanceVariableNames: instVarString [ 
	
	^ self slots: instVarString asSlotCollection 
]

{ #category : #testing }
TraitedMetaclass >> isAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my or in another composition somewhere deeper in 
	the tree of traits compositions."

	^ self traitComposition isAliasSelector: aSymbol
]

{ #category : #testing }
TraitedMetaclass >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^ self traitComposition isLocalAliasSelector: aSymbol
]

{ #category : #testing }
TraitedMetaclass >> isLocalMethodsProtocol: aProtocol [
	aProtocol methods ifEmpty: [ ^true ].
	
	^aProtocol methods anySatisfy: [ :each | self isLocalSelector: each]
]

{ #category : #testing }
TraitedMetaclass >> isLocalSelector: aSelector [
	
	^ localMethods includesKey: aSelector
]

{ #category : #testing }
TraitedMetaclass >> isRejectedMethod: e [
	^ (localMethods includesKey: e) or: [ (Class canUnderstand: e) and: [ (TraitedClass methodDict includesKey: e) not ] ] 
]

{ #category : #testing }
TraitedMetaclass >> isSelectorToKeep: e [
	^ (localMethods includesKey: e) or: [ TraitedClass methodDict includesKey: e ].
]

{ #category : #accessing }
TraitedMetaclass >> localMethodDict [
	^ localMethods
]

{ #category : #accessing }
TraitedMetaclass >> localMethods [
	"returns the methods of classes excluding the ones of the traits that the class uses" 
	 
	^ localMethods values
]

{ #category : #accessing }
TraitedMetaclass >> localSelectors [
	
	^ localMethods keys
]

{ #category : #adding }
TraitedMetaclass >> new [
	| newClass |
	newClass := super new.
	^newClass.
]

{ #category : #testing }
TraitedMetaclass >> originMethod: aMethod [

	(self localMethodDict includesKey: aMethod selector)
		ifTrue: [ ^ self compiledMethodAt: aMethod selector].

	(aMethod hasProperty: #traitSource)
		ifTrue: [ |newSelector|
			newSelector := self traitComposition originSelectorOf: aMethod selector.
			^ (aMethod propertyAt: #traitSource) compiledMethodAt: newSelector ifAbsent: [aMethod] ].

	^ (self traitComposition
		traitDefining: aMethod selector
		ifNone: [ self class traitedClassTrait traitDefining: aMethod selector ifNone: [ self ] ]) 
		compiledMethodAt: aMethod selector ifAbsent: [ ^ aMethod ]		

]

{ #category : #testing }
TraitedMetaclass >> originOfMethod: aMethod [

	(aMethod hasProperty: #traitSource) 
		ifTrue: [ ^ (aMethod propertyAt: #traitSource) innerClass ]. 

	(localMethods includesKey: aMethod selector)
		ifTrue: [ ^ self ].

	^ (self traitComposition
		traitDefining: aMethod selector
		ifNone: [ self class traitedClassTrait traitDefining: aMethod selector ifNone: [ ^ self ] ]) innerClass
]

{ #category : #initialization }
TraitedMetaclass >> rebuildMethodDictionary [
	| selectors removedSelectors modified |
	
	modified := false.
	
	methodDict valuesDo: [ :m | (m propertyAt: #traitSource) ifNil: [ localMethods at: m selector put: m ]].
	
	selectors := self traitComposition selectors reject: [ :e | self isRejectedMethod: e ].
	selectors do: [ :e | modified := modified | (self traitComposition installSelector: e into: self replacing: false) ].
	
	removedSelectors := methodDict keys reject: [ :aSelector | (selectors includes: aSelector) or: [ self isSelectorToKeep: aSelector ] ].
	modified := modified | (removedSelectors isNotEmpty).
	removedSelectors do: [ :aSelector | methodDict removeKey: aSelector ].
	removedSelectors do: [ :aSelector | self organization removeElement: aSelector ]. 
	
	^ modified
]

{ #category : #categories }
TraitedMetaclass >> recategorizeSelector: selector from: oldCategory to: newCategory [
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

{ #category : #traits }
TraitedMetaclass >> removeFromComposition: aTrait [ 
	self setTraitComposition: (self traitComposition copyWithoutTrait: aTrait asTraitComposition)
]

{ #category : #'accessing method dictionary' }
TraitedMetaclass >> removeSelector: aSelector [

	self localMethodDict removeKey: aSelector ifAbsent: [  ].
	super removeSelector: aSelector.
	TraitChange removeSelector: aSelector on: self.
]

{ #category : #initialization }
TraitedMetaclass >> slots [

	^ super slots reject: [ :e | composition slots includes: e ]
]

{ #category : #'initialize-release' }
TraitedMetaclass >> slots: slotCollection [ 

	| theClass |
	theClass := self theNonMetaClass.
		
	theClass := theClass classInstaller update: theClass to: [ :builder |
		builder
			fillFor: theClass;
			classSlots: slotCollection ].
	^ theClass theMetaClass
]

{ #category : #accessing }
TraitedMetaclass >> slotsWithoutTraits [
	^ self slots reject: [ :e | TraitedClass slots anySatisfy: [ :x | x name = e name ] ] 
]

{ #category : #accessing }
TraitedMetaclass >> traitComposition [
	^ composition
]

{ #category : #accessing }
TraitedMetaclass >> traitComposition: aComposition [

	composition := aComposition
]

{ #category : #accessing }
TraitedMetaclass >> traitCompositionString [
	^ self traitComposition asString
]

{ #category : #accessing }
TraitedMetaclass >> traitUsers [

	^ #()
]

{ #category : #initialization }
TraitedMetaclass >> traits [
	^ composition traits
]
