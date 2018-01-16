"
An RG2BehaviorDefinition is an abstract definition for class-alike entities (e.g. classes, traits)

Instance Variables
	methods:		<Collection>
	protocols:		<Collection>
	superclass:		<Object>
"
Class {
	#name : #RG2BehaviorDefinition,
	#superclass : #RG2Definition,
	#instVars : [
		'superclass',
		'localMethods',
		'protocols',
		'traitComposition',
		'metaclass',
		'tagsForMethods',
		'tags'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition class >> named: aString [

	^ super named: aString asSymbol
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition class >> named: aString parent: aRG2Definition [

	^ super named: aString asSymbol parent: aRG2Definition
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> addLocalMethod: aRG2MethodDefinition [ 

	self backend forBehavior addLocalMethod: aRG2MethodDefinition to: self.
	
	self announcer methodAdded: aRG2MethodDefinition.

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> addMethodTag: aSymbol [ 

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior addMethodTag: aSymbol to: self. ].
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> addProtocol: aSymbol [ 

	self addMethodTag: aSymbol

]

{ #category : #'managing container' }
RG2BehaviorDefinition >> addoptToParentStub [

	| aPackage |
	
	self isRing2Resolved ifFalse: [ ^ self ].

	super addoptToParentStub.

	self parent pvtAddBehavior: self.
	


]

{ #category : #'queries - other' }
RG2BehaviorDefinition >> allInstVarNames [
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter."

	| vars |
	(self superclass == self or: [ self superclass isNil ])
		ifTrue: [vars := self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [vars := self superclass allInstVarNames , self instVarNames].
	^vars
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> allSlots [

	^ {  }
]

{ #category : #announcements }
RG2BehaviorDefinition >> announceDefinitionChangeDuring: aBlock [

	| oldVersion |

	self announcer isSuspended ifTrue: [ 
		aBlock value. 
		^ self ]	.
		
	self announcer  suspendAllWhile: [ 
		oldVersion := self copyForBehaviorDefinition.].
	aBlock value.
	self announcer 	behaviorDefinitionChangedFrom: oldVersion to: self.
	self announcer behaviorModificationAppliedTo: self.

	
	
]

{ #category : #asYetUnclassified }
RG2BehaviorDefinition >> asYetUnclassifiedProtocolName [

	^ 'as yet unclassified' asSymbol
]

{ #category : #'queries - other' }
RG2BehaviorDefinition >> baseBehavior [

	^ self 
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> baseClass [

	^ self.
]

{ #category : #binding }
RG2BehaviorDefinition >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"

	^ self isRootInEnvironment
		ifFalse: [self superclass bindingOf: varName]
		ifTrue: [ nil ]
]

{ #category : #testing }
RG2BehaviorDefinition >> canMergeWith: aRG2Definition visited: visitedDefinitions [

	| newVisited |

	(visitedDefinitions includes: self) ifTrue: [ ^ true ].
	
	(self isRing2FullyResolved or: [ aRG2Definition isRing2FullyResolved]) ifTrue: [^ true].

	(super canMergeWith: aRG2Definition visited: visitedDefinitions) ifFalse: [ ^ false ].

	newVisited := visitedDefinitions copyWith: self.
	
	self superclass ~~ self ifTrue: [ 
		(self superclass canMergeWith: aRG2Definition superclass visited: newVisited) ifFalse: [ ^ false ]].

	self metaclass ~~ self ifTrue: [ 
		(self metaclass canMergeWith: aRG2Definition metaclass visited: newVisited) ifFalse: [ ^ false ]].
	
	^ true.
]

{ #category : #accessing }
RG2BehaviorDefinition >> category [

	"return only because of tests thet use RG2BehaviorDefinition directly"
	
	^ String new
]

{ #category : #children }
RG2BehaviorDefinition >> childDefinitionsDo: aBlock traversed: aSet [

	self pvtSuperclass withChildrenDo: aBlock traversed: aSet.
	self pvtLocalMethodsDo: [ :each | each withChildrenDo: aBlock traversed: aSet ].
	self pvtTraitComposition withChildrenDo: aBlock traversed: aSet.
	self pvtMetaclass withChildrenDo: aBlock traversed: aSet.
	
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> classSide [

	^ self
]

{ #category : #'accessing instances and variables' }
RG2BehaviorDefinition >> classVarNames [
	"Answer a collection of the receiver's class variable names."

	^#()
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> classVariableDefinitionString [

	"TODO !"
	^ ''
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> classVariablesBindings [

	^ self propertyNamed: #classVariablesBindings ifAbsentPut: [ IdentityDictionary new.]
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> cleanLocalMethods [

	| oldMethods |
	
	oldMethods := self localMethods.

	self backend forBehavior cleanLocalMethodsFor: self.

	self cleanProtocols.
	
	oldMethods do: [ :each |
		self announcer methodRemoved: each ]

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> cleanProtocols [
	
	self cleanTagsForMethods
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> cleanTags [

	self announceDefinitionChangeDuring: [ 
		self cleanTagsWithoutAnnouncemnt ].
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> cleanTagsForMethods [

	self backend forPackage cleanTagsForMethodsFor: self
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> cleanTagsWithoutAnnouncemnt [

	self backend forBehavior cleanClassTagsFor: self.
]

{ #category : #compiling }
RG2BehaviorDefinition >> compile: aSourceCode classified: aProtocolName [

	| newMethod selector originalMethod |
	newMethod := RG2MethodDefinition parent: self.
	self addLocalMethod: newMethod.
	newMethod sourceCode: aSourceCode.
	selector := newMethod ast selector.
	originalMethod := self localMethodNamed: selector ifAbsent: [ nil ].
	originalMethod ifNotNil: [ 
		self removeLocalMethod: originalMethod].
	newMethod name: newMethod ast selector.
	newMethod tagWith: aProtocolName.
	
	^ newMethod

]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> compiler [

	^ self class compiler
]

{ #category : #copying }
RG2BehaviorDefinition >> copyForBehaviorDefinitionPostCopy [

	super copyForBehaviorDefinitionPostCopy.
	
	superclass := self superclass shallowCopy. "we need the superclass name"
	traitComposition := self traitComposition copyForBehaviorDefinition.
	tagsForMethods := self tagsForMethods copy asSet.
	tags := self tags copy asSet.
	

]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> correspondingObjectIn: aRG2Environment [

	(self isRing2Resolved not and: ((self hasPropertyNamed: #creator) and: [ self hasPropertyNamed: #role])) ifTrue: [ 
		| creator role base correspondingBase correspondingCreator |
		creator := self propertyNamed: #creator.
		creator == self ifFalse: [  
			role := self propertyNamed: #role. 
			base := creator isTrait 
				ifFalse: [ creator isClass ifTrue: [ creator ] ifFalse: [ creator baseClass ]]
				ifTrue: [ creator isMetaclassTrait ifFalse: [ creator ]	ifTrue: [ creator baseTrait ]].
			base == self ifFalse: [ 
				correspondingBase := base correspondingObjectIn: aRG2Environment.
				correspondingBase ifNil: [ ^ nil ].
				(role = #class) ifTrue: [ ^ correspondingBase ].
				(role = #metaclass) ifTrue: [ ^ correspondingBase metaclass ].
				(role = #superclass) ifTrue: [ ^ correspondingBase superclass ].
				(role = #superclassMetaclass) ifTrue: [ ^ correspondingBase superclass metaclass ].
				
				(role = #trait) ifTrue: [ ^ correspondingBase ].
				(role = #classTrait) ifTrue: [ ^ correspondingBase classTrait ].
				(role = #traitMetaclass) ifTrue: [ ^ correspondingBase metaclass ].
				(role = #classTraitMetaclass) ifTrue: [ ^ correspondingBase classTrait metaclass ]].
			
			"cannot found corresponding base, try corresponding creator"
			correspondingCreator := creator correspondingObjectIn: aRG2Environment.
			correspondingCreator ifNotNil: [
				self == creator ifTrue: [ ^ correspondingCreator ].
				self metaclass = creator ifTrue: [ ^ correspondingCreator baseClass ].
				"for superclass and superclass metaclass we are not sure which subclass is the right one. Skip next roles."
				self isTrait ifTrue: [ 
					self classTrait = creator ifTrue: [ ^ correspondingCreator baseTrait ] ]
		]]].
		

	^ aRG2Environment ask behaviorNamed: self name.  
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultLayout [

	^ self defaultFixedLayoutStubIn: self.
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultLocalMethods [

	^ Set new
]

{ #category : #'managing container' }
RG2BehaviorDefinition >> defaultParentStub [

	^ self defaultEnvironmentStub
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultProtocols [

	^ Set new
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultTags [

	^ Set new
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultTagsForMethods [

	^ Set new
]

{ #category : #'default model values' }
RG2BehaviorDefinition >> defaultTraitComposition [

	^ self defaultTraitCompositionStubIn: self.
]

{ #category : #accessing }
RG2BehaviorDefinition >> definitionForNautilus [

	^ self name asString
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> ensureLocalMethodNamed: aSymbol [

	^ self localMethodNamed: aSymbol ifAbsent: [ 
		| newMethod |
		newMethod := RG2MethodDefinition named: aSymbol asSymbol parent: self.
		self addLocalMethod: newMethod.
		newMethod].
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> ensureMethodTagNamed: aSymbol [

	^ self tagsForMethods detect: [ :each | each asSymbol = aSymbol ] ifNone: [ 
		self addMethodTag: aSymbol.
		aSymbol ]
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> ensureProtocolNamed: aSymbol [
 
	^ self ensureMethodTagNamed: aSymbol
]

{ #category : #documentation }
RG2BehaviorDefinition >> extensions [

	^ self localMethods select: [ :each | each package ~= self package ]
]

{ #category : #'queries - testing' }
RG2BehaviorDefinition >> hasMethods [
	"validates the existance of methods"

	^ self methods notEmpty
]

{ #category : #'queries - testing' }
RG2BehaviorDefinition >> hasTraitComposition [

	^	self traitComposition transformations isEmpty not
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> includesSelector: aString [

	^ self selectors includes: aString
]

{ #category : #'testing class hierarchy' }
RG2BehaviorDefinition >> inheritsFrom: aClass [ 
	"Answer whether the argument, aClass, is on the receiver's superclass 
	chain."

	| aSuperclass |
	aSuperclass := self superclass.
	[aSuperclass == nil]
		whileFalse: 
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass := aSuperclass superclass].
	^false
]

{ #category : #initialization }
RG2BehaviorDefinition >> initialize [

	super initialize.
	
	"use unresolved value for superclass to avoid infinite recursion. It needs to be set later"
	superclass := RG2UnresolvedValue recursive.
	localMethods := self unresolvedValue: self defaultLocalMethods.
	protocols := self unresolvedValue: self defaultProtocols.
	traitComposition := self unresolvedValue: self defaultTraitComposition.
	tagsForMethods := self unresolvedValue: self defaultTagsForMethods.	
	tags := self unresolvedValue: self defaultTags.	

]

{ #category : #initialization }
RG2BehaviorDefinition >> initializeUnresolved [

	super initializeUnresolved.

	superclass := self. "will be set later"
	localMethods := self unresolvedValue: self defaultLocalMethods.
	protocols := self unresolvedValue: self defaultProtocols.
	traitComposition := self unresolvedValue: self defaultTraitComposition.
	tagsForMethods := self unresolvedValue: self defaultTagsForMethods.	
	tags := self unresolvedValue: self defaultTags.	

]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> instSize [

	^ 0
]

{ #category : #'queries - other' }
RG2BehaviorDefinition >> instVarNames [

	^ Array new
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> instanceSide [
	^ self 
]

{ #category : #'queries - testing' }
RG2BehaviorDefinition >> isMeta [
	"By default a non-meta class is considered"

	^false
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> isReferencedIn: aRG2MethodDefinition [ 

	^ (aRG2MethodDefinition ast variableNodes select: #isGlobal) anySatisfy: [ :each | each name = self name ] 
]

{ #category : #'queries - testing' }
RG2BehaviorDefinition >> isRootInEnvironment [		
		
	^ self superclass == self or: [ self superclass isNil ] 
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> localMethodNamed: aSymbol ifAbsent: aBlock [

	self localMethodsDo: [ :each | (each name = aSymbol) ifTrue: [^ each]].
	
	^ aBlock value.
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> localMethods [

	| methods |
	
	methods := IdentitySet new.
	self localMethodsDo: [ :each | methods add: each ].
	^ methods asArray
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> localMethodsDo: aBlock [

	self backend forBehavior localMethodsFor: self do: aBlock
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> localSelectors [

	^ self localMethods collect: #name
]

{ #category : #resolving }
RG2BehaviorDefinition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	super makeMinimallyResolved.
	
	superclass := self superclass makeMinimallyResolved.
	traitComposition := self traitComposition makeMinimallyResolved.
	metaclass := self metaclass makeMinimallyResolved.
	tags := self tags asSet.	

]

{ #category : #resolving }
RG2BehaviorDefinition >> makeResolved [

	super makeResolved.
	
	self resolveSuperclass.
	self resolveLocalMethods.
	self resolveProtocols.
	self resolveTraitComposition.
	self resolveMetaclass.
	self resolveTagsForMethods.
	self resolveTags.

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> metaclass [

	^ self backend forBehavior metaclassFor: self
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> metaclass: aRG2MetaclassDefinition [

	self backend forBehavior setMetaclassFor: self to: aRG2MetaclassDefinition.
	

]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> methodNamed: aSymbol [

	| allMethods |
	allMethods := IdentitySet new.
	self methods do: [ :each | each name = aSymbol ifTrue: [^ each]].
	^ nil
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> methods [

	| methodsFromTraits methodsDict |
	"TODO: make nicer"
	"^ self propertyNamed: #methods ifAbsentPut: [ "
	methodsFromTraits := self traitComposition methods collect:  [ :each | each copy parent: self ].
	methodsDict := Dictionary new.
	methodsFromTraits do: [ :each |
		methodsDict at: each name put: each ].
	self localMethodsDo: [ :each |
		methodsDict at: each name put: each ].
	^ methodsDict values asArray"]"

]

{ #category : #'queries - tags' }
RG2BehaviorDefinition >> methodsTaggedWith: aSymbol [

	^ self localMethods select: [ :each | each isTaggedWith: aSymbol ]
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> name: aString [

	| oldName subclassesWithOldDefinitions |
	
	oldName := self name.
	
	subclassesWithOldDefinitions := (self subclasses reject: #isMeta) collect: [ :each | 
		each -> each copyForBehaviorDefinition ].
	
	self announceDefinitionChangeDuring: [ 
		super name: aString ].
	
	self announcer behaviorRenamed: self from: oldName.

	subclassesWithOldDefinitions do: [ :assoc |
		self announcer behaviorDefinitionChangedFrom: assoc value to: assoc key.
		self announcer behaviorModificationAppliedTo: assoc key.
		self announcer behaviorParentRenamed: assoc key from: oldName ].
]

{ #category : #printing }
RG2BehaviorDefinition >> printOn: aStream [ 
	aStream nextPutAll: self name
]

{ #category : #'queries - protocols' }
RG2BehaviorDefinition >> protocols [

	| methodTags |
	
	methodTags := self tagsForMethods.
	^	methodTags
		ifEmpty: [ 
			self methods 
				ifEmpty: [ OrderedCollection new ]
				ifNotEmpty: [  OrderedCollection with: self class asYetUnclassifiedProtocolName   ].
			]
		ifNotEmpty: [ methodTags  ]

]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtAddLocalMethod: aRG2MethodDefinition [

	self environment verifyOwnership: aRG2MethodDefinition.
	
	localMethods isRing2Resolved ifFalse: [ 
		self pvtCleanLocalMethods ].

	(self hasResolved: #tagsForMethods) 
		ifFalse: [ self pvtCleanTagsForMethods ].

	aRG2MethodDefinition tags do: [ :aTag |
		self pvtAddMethodTag: aTag ].

	localMethods add: aRG2MethodDefinition.	
	
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtAddMethodTag: aSymbol [
	
	tagsForMethods isRing2Resolved ifFalse: [ 
		self pvtCleanTagsForMethods  ].
	
	tagsForMethods add: aSymbol.	
	
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtCleanLocalMethods [

	localMethods := self defaultLocalMethods.
	
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtCleanTags [

	tags := self defaultTags.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtCleanTagsForMethods [

	tagsForMethods := self defaultTagsForMethods.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #private }
RG2BehaviorDefinition >> pvtLocalMethods: aCollection [

	"use only for low-level copying"
	localMethods := aCollection
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtLocalMethodsDo: aBlock [

	^ localMethods value do: aBlock
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtMetaclass [

	^ metaclass value
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtMetaclass: aRG2MetaclassDefinition [

	self environment verifyOwnership: aRG2MetaclassDefinition.

	^ metaclass := aRG2MetaclassDefinition
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtRemoveLocalMethod: aRG2MethodDefinition [

	self environment verifyOwnership: aRG2MethodDefinition.
	
	localMethods remove: aRG2MethodDefinition.
	

]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtRemoveMethodTag: aSymbol [
	
	tagsForMethods remove: aSymbol.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#superclass -> superclass. 
		#localMethods -> localMethods. 
		#protocols -> protocols. 
		#traitComposition -> traitComposition.  
		#metaclass -> metaclass.  
		#tagsForMethods -> tagsForMethods. 
 		#tags -> tags. 
    	}
	
	
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtSuperclass [

	^ superclass value
	
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtSuperclass: aRG2BehaviorDefinition [

	aRG2BehaviorDefinition ifNotNil: [ 
		self environment verifyOwnership: aRG2BehaviorDefinition.].

	^ superclass := aRG2BehaviorDefinition
	
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtTagWith: aSymbol [
	
	tags isRing2Resolved ifFalse: [ 
		self pvtCleanTags  ].
	
	tags add: aSymbol.	
	
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtTags: aCollection [

	^ tags := aCollection
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtTagsDo: aBlock [

	^ tags value do: aBlock
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtTagsForMethodsDo: aBlock [

	^ tagsForMethods value do: aBlock
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtTraitComposition [

	^ traitComposition value
]

{ #category : #'private - backend interface' }
RG2BehaviorDefinition >> pvtTraitComposition: aRG2TraitCompositionDefinition [

	self environment verifyOwnership: aRG2TraitCompositionDefinition.

	^ traitComposition := aRG2TraitCompositionDefinition
]

{ #category : #'private - backend access' }
RG2BehaviorDefinition >> pvtUntagFrom: aSymbol [
	
	(tags value includes: aSymbol)
		ifTrue: [ tags remove: aSymbol ].
	
	"TODO:Announce"
	
	
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> referencedBehaviors [

	^ { self superclass. self metaclass}, self traitComposition referencedBehaviors
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> referencedPackages [

	^ Array new
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> removeLocalMethod: aRG2MethodDefinition [ 

	(aRG2MethodDefinition package = self package)
		ifFalse: [ aRG2MethodDefinition package removeExtensionMethod:  aRG2MethodDefinition ].
		
	self backend forBehavior removeLocalMethod: aRG2MethodDefinition from: self.
	
	self announcer methodRemoved: aRG2MethodDefinition.

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> removeMethodTag: aSymbol [

	self backend forPackage removeMethodTag: aSymbol from: self.
	
	self localMethodsDo: [ :method | 
		method untagFrom: aSymbol ].
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> removeProtocol: aSymbol [

	self removeMethodTag: aSymbol.
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.
	
	self resolveSuperclassName.
	
	self resolveTraitComposition.
	self traitComposition resolveBehaviorInformation.
	
	self resolveSibling.
	self resolveTags.
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveLocalMethods [

	localMethods := self localMethods asSet.
	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveMetaclass [

	metaclass := self metaclass.
	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveName [

	"try to set the correct name if sibling has already a name"
	
	( self sibling isRing2Resolved and: [ self sibling hasResolved: #name ] ) ifTrue: [ 
		self setNameFromSiblingIfNeeded ].

]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveProtocols [

	protocols := self protocols.

]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveSibling [

	self subclassResponsibility.
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveSuperclass [

	superclass := self superclass.
	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveSuperclassName [

	self resolveSuperclass.
	self superclass resolveName.	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveTags [

	tags := self tags asSet.	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveTagsForMethods [

	tagsForMethods := self tagsForMethods asSet.
	
]

{ #category : #resolving }
RG2BehaviorDefinition >> resolveTraitComposition [

	traitComposition := self traitComposition.
	
]

{ #category : #'queries - methods' }
RG2BehaviorDefinition >> selectors [

	^ self methods collect: #name
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> sibling [

	^ self subclassResponsibility.
]

{ #category : #printing }
RG2BehaviorDefinition >> storeOn: aStream [ 
	self name storeOn: aStream
]

{ #category : #'accessing class hierarchy' }
RG2BehaviorDefinition >> subclasses [

	| result |
	result := IdentitySet new.
	self environment behaviorsDo: [ :each |
		(each superclass == self) ifTrue: [ result add: each] ].
	^ result asArray
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> superclass [

	^ self backend forBehavior superclassFor: self
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> superclass: aRG2BehaviorDefinition [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setSuperclassFor: self to: aRG2BehaviorDefinition ].
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> tagWith: aSymbol [ 

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior tagClass: self with: aSymbol.
		self package addClassTag: aSymbol.
	].
	
]

{ #category : #'accessing - model' }
RG2BehaviorDefinition >> tags [

	| allTags |
	allTags := IdentitySet new.
	self tagsDo: [ :each | allTags add: each].
	^ allTags asArray 

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> tagsDo: aBlock [

	self backend forBehavior tagsForClass: self do: aBlock
]

{ #category : #'accessing - model' }
RG2BehaviorDefinition >> tagsForMethods [
	"Retrieves the traits defined in the receiver"

	| allTags |
	allTags := IdentitySet new.
	self tagsForMethodsDo: [ :each | allTags add: each].
	^ allTags asArray

]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> tagsForMethodsDo: aBlock [

	self backend forBehavior tagsForMethodsFor: self do: aBlock
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> theMetaClass [
	^self
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> theNonMetaClass [
	^self
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> traitComposition [

	^ self backend forBehavior traitCompositionFor: self
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> traitComposition: aRG2TraitCompositionDefinition [

	self announceDefinitionChangeDuring: [ 
		self backend forBehavior setTraitCompositionFor: self to: aRG2TraitCompositionDefinition.].
]

{ #category : #'as yet unclassified' }
RG2BehaviorDefinition >> traitCompositionString [
	^ self traitComposition traitCompositionString
		
]

{ #category : #'accessing - backend' }
RG2BehaviorDefinition >> untagFrom: aSymbol [

	self backend forPackage untagClass: self from: aSymbol
]

{ #category : #asYetUnclassified }
RG2BehaviorDefinition >> usedTraits [

	^ self traitComposition usedTraits
]
