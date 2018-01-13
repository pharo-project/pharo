"
I am the root class of the Ring meta-model.
A Ring definition has a name, properties and knows its environment
"
Class {
	#name : #RG2Definition,
	#superclass : #Object,
	#instVars : [
		'properties',
		'name',
		'parent'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'instance creation' }
RG2Definition class >> asYetUnclassifiedProtocolName [

	^ 'as yet unclassified' asSymbol
]

{ #category : #'instance creation' }
RG2Definition class >> named: aString [

	"create model in its own environment"

	^self new
		pvtName: aString;
		yourself
]

{ #category : #'instance creation' }
RG2Definition class >> named: aName parent: aRG2Definition [

	^self basicNew
		parent: aRG2Definition;
		initialize;
		pvtName: aName asSymbol;
		yourself
]

{ #category : #'instance creation' }
RG2Definition class >> parent: aRG2Definition [

	^self basicNew
		parent: aRG2Definition;
		initialize;
		yourself
]

{ #category : #'instance creation' }
RG2Definition class >> unnamed [

	^self new
]

{ #category : #'instance creation' }
RG2Definition class >> unresolved [

	^ self basicNew
		initializeUnresolved;
		yourself.
]

{ #category : #'instance creation' }
RG2Definition class >> unresolvedNamed: aString withParent: aRG2Defintion [

	^self basicNew
		initializeUnresolved; 
		pvtName: aString asSymbol;
		parent: aRG2Defintion;
		yourself
]

{ #category : #'instance creation' }
RG2Definition class >> unresolvedWithParent: aRG2Defintion [

	^self basicNew
		initializeUnresolved; 
		parent: aRG2Defintion;
		yourself
]

{ #category : #'managing container' }
RG2Definition >> addoptToParentStub [
]

{ #category : #'as yet unclassified' }
RG2Definition >> announce: anAnnouncement [

	self environment announcer announce: anAnnouncement
]

{ #category : #'as yet unclassified' }
RG2Definition >> announcer [

	^ self environment announcer
]

{ #category : #converting }
RG2Definition >> asRing2Definition [

	^ self
]

{ #category : #accessing }
RG2Definition >> ask [

	^ self 
]

{ #category : #'as yet unclassified' }
RG2Definition >> authorForStamp: stamp [
	
	^ stamp isEmptyOrNil
		ifTrue: [ '<historical>']
		ifFalse: [ stamp substrings first].
]

{ #category : #accessing }
RG2Definition >> backend [
	
	^ self environment backend
]

{ #category : #merging }
RG2Definition >> canMergeSimplyWith: aRG2Definition [

	^ (self resolvedProperties intersection: aRG2Definition resolvedProperties) isEmpty.
]

{ #category : #merging }
RG2Definition >> canMergeSimplyWithoutName: aRG2Definition [

	| intersection |
	
	intersection := self resolvedProperties intersection: aRG2Definition resolvedProperties.
	^ intersection isEmpty or: [intersection = #(name)]
]

{ #category : #merging }
RG2Definition >> canMergeWith: aRG2Definition [

	^ (self nameOrNil == aRG2Definition nameOrNil)

]

{ #category : #merging }
RG2Definition >> canMergeWith: aRG2Definition visited: visitedDefinitions [

	(visitedDefinitions includes: self) ifTrue: [ ^ true ].

	(self hasResolvedName not or: [ aRG2Definition hasResolvedName not]) ifTrue: [ ^ true ].
	
	^ (self nameOrNil == aRG2Definition nameOrNil)

]

{ #category : #'as yet unclassified' }
RG2Definition >> child: aChildDefinition renamedFrom: oldName to: newName [
]

{ #category : #children }
RG2Definition >> childDefinitionsDo: aBlock traversed: aSet [
]

{ #category : #properties }
RG2Definition >> cleanedProperties [

	^ properties := IdentityDictionary new
]

{ #category : #'as yet unclassified' }
RG2Definition >> compatibleWith: aRG2Definition [

	(self class = aRG2Definition class)
		ifFalse: [ ^ false ].
		
	(self canMergeSimplyWith: aRG2Definition) ifTrue: [ ^ true ].
		
	^ (self name = aRG2Definition name)
]

{ #category : #copying }
RG2Definition >> copyForBehaviorDefinition [

	"returns copy of the behavior model that contains only information related to the behavior 
	definition"
	
	^ self shallowCopy copyForBehaviorDefinitionPostCopy
]

{ #category : #copying }
RG2Definition >> copyForBehaviorDefinitionPostCopy [

	super postCopy.
	
	
	
	
]

{ #category : #'as yet unclassified' }
RG2Definition >> copyForEnvironment: aRG2Environment [

	| newDefinition |
	
	newDefinition := self copy.
	newDefinition propertyNamed: #environment put: aRG2Environment.
	newDefinition propertyNamed: #originalDefinition put: self.
	^ newDefinition

]

{ #category : #properties }
RG2Definition >> copyPropertiesFrom: aRG2Definition [

	properties := aRG2Definition properties copy.
]

{ #category : #copying }
RG2Definition >> copyWithParent: newParent [

	"create copy of the definition and then assign the new parent"
	
	^ self copy 
		parent: newParent;
		yourself.
]

{ #category : #'as yet unclassified' }
RG2Definition >> correspondingObjectIn: aRG2Environment [

	"find suitable object that corresponds to the definition in the oher environmnet aRG2Environment"
	self subclassResponsibility.
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultClassStub [

	^ RG2ClassDefinition unresolved
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultCommentStubIn: aRG2BehaviorDefinition [

	^ RG2CommentDefinition unresolved
		parent: aRG2BehaviorDefinition;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultEnvironmentStub [

	^ RG2Environment unresolved
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultFixedLayoutStub [

	^ RG2FixedLayoutDefinition unresolved
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultFixedLayoutStubIn: aRG2BehaviorDefinition [

	^ RG2FixedLayoutDefinition unresolved
		parent: aRG2BehaviorDefinition;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultPackageStubIn: aRG2Environment [

	^ RG2PackageDefinition unresolved
		parent: aRG2Environment;
		yourself.
]

{ #category : #'managing container' }
RG2Definition >> defaultParentStub [

	self shouldBeImplemented
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultTraitCompositionStub [

	^ RG2TraitCompositionDefinition unresolved
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defaultTraitCompositionStubIn: aRG2BehaviorDefinition [

	^ RG2TraitCompositionDefinition unresolved
		parent: aRG2BehaviorDefinition;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RG2Definition >> defautlParentStub [

	"creates parent lazily if the real parent is not known"
	
	self subclassResponsibility 
]

{ #category : #accessing }
RG2Definition >> environment [
	
	^ self propertyNamed: #environment ifAbsent: [ self parent environment ].
]

{ #category : #'as yet unclassified' }
RG2Definition >> findSuitableParentIn: aRG2Environment [

	^ self subclassResponsibility 
]

{ #category : #'as yet unclassified' }
RG2Definition >> fullCopy [

	^ self copy.
]

{ #category : #accessing }
RG2Definition >> fullName [

	^self name
]

{ #category : #properties }
RG2Definition >> hasProperties [

	^ properties isEmptyOrNil not
]

{ #category : #properties }
RG2Definition >> hasPropertyNamed: propertyName [
	"Answer <true> if the receiver contains the property named <propertyName>. "
	
	^ self hasProperties
		ifTrue: [ properties includesKey: propertyName ]	
		ifFalse:[ false ].

]

{ #category : #resolving }
RG2Definition >> hasResolved: propertySymbol [

	^ self resolvedProperties includes: propertySymbol



]

{ #category : #resolving }
RG2Definition >> hasResolvedAll: aCollectionOfSymbols [

	^ self resolvedProperties includesAll: aCollectionOfSymbols



]

{ #category : #testing }
RG2Definition >> hasResolvedName [		
	
	^ self hasResolved: #name
]

{ #category : #resolving }
RG2Definition >> hasUnresolved: propertySymbol [

	^ (self resolvedProperties includes: propertySymbol) not



]

{ #category : #resolving }
RG2Definition >> hasUnresolvedAll: aCollectionOfSymbols [

	^ (self resolvedProperties includesAll: aCollectionOfSymbols) not



]

{ #category : #initialization }
RG2Definition >> initialize [

	super initialize.

	name := self unresolvedValue: self unresolvedName.


]

{ #category : #initialization }
RG2Definition >> initializeUnresolved [

	self propertyNamed: #resolved put: false. 
	name := self unresolvedValue: self unresolvedName.
	


]

{ #category : #'testing types' }
RG2Definition >> isBehavior [

	^ false
]

{ #category : #resolving }
RG2Definition >> isBehaviorInformationResolved [

	^ self propertyNamed: #behaviorInformationResolved ifAbsent: [ false ].

]

{ #category : #'testing types' }
RG2Definition >> isClass [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isComment [

	^false
]

{ #category : #'testing types' }
RG2Definition >> isEnvironment [

	^false
]

{ #category : #'testing types' }
RG2Definition >> isGlobalVariable [

	^false
]

{ #category : #'testing types' }
RG2Definition >> isLayout [

	^false
]

{ #category : #'as yet unclassified' }
RG2Definition >> isMeta [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isMetaclass [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isMetaclassTrait [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isMethod [

	^false
]

{ #category : #'testing types' }
RG2Definition >> isPackage [

	^false
]

{ #category : #'testing types' }
RG2Definition >> isProtocol [

	^false
]

{ #category : #resolving }
RG2Definition >> isRing2FullyResolved [

	^ self backend hasFullyResolved: self
]

{ #category : #resolving }
RG2Definition >> isRing2FullyUnresolved [		
	
	^ self backend hasFullyUnresolved: self 		


]

{ #category : #testing }
RG2Definition >> isRing2Object [

	^true
]

{ #category : #testing }
RG2Definition >> isRing2Resolved [

	^ self propertyNamed: #resolved ifAbsent: [ true ]
]

{ #category : #'testing types' }
RG2Definition >> isSlot [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isTraitAlias [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isTraitComposition [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isTraitExclusion [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isTraitTransformation [

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isVariable [

	"is the definition a variable class?"

	^ false
]

{ #category : #'testing types' }
RG2Definition >> isVariableDefinition [

	^ false
]

{ #category : #resolving }
RG2Definition >> makeMinimallyResolved [

	self isRing2Resolved ifTrue: [ ^ self ].
	self propertyNamed: #resolved put: true.
	
	name := self name.
	parent := self parent makeMinimallyResolved.
]

{ #category : #resolving }
RG2Definition >> makeResolved [

	self propertyNamed: #resolved put: true.
	
	self resolveName
]

{ #category : #merging }
RG2Definition >> merge: aRG2Definition [

	| sourceHasResolvedName |
	
	sourceHasResolvedName := aRG2Definition hasResolved: #name.
	
	((self hasResolved: #name) 
		and: ( sourceHasResolvedName 
		and: [ self name ~~ aRG2Definition name]))
			ifTrue: [ RG2MergeError property: #name target: self source: aRG2Definition ].
	
	sourceHasResolvedName
		ifTrue: [ self name: aRG2Definition name ].


]

{ #category : #accessing }
RG2Definition >> name [

	^ (self backend nameFor: self) orDefaultForUnresolved 
]

{ #category : #accessing }
RG2Definition >> name: aString [

	| oldName result |
	
	oldName := name.
	result := self backend setNameFor: self to: aString.
	self parent child: self renamedFrom: oldName to: aString.
	^ result
]

{ #category : #'as yet unclassified' }
RG2Definition >> nameOrNil [

	"...if name is unresolved"
		
	(self hasResolved: #name) ifFalse: [ ^ nil ].

	^ self name

]

{ #category : #'as yet unclassified' }
RG2Definition >> package [

	^ self parent package
]

{ #category : #accessing }
RG2Definition >> parent [
	"The parent of a class definition element: method, comment and variable is the class definition. This method retrieves the class that defines such element"
	
	parent ifNil: [ 
		self parent: self defaultParentStub.
		self addoptToParentStub ].
	
	^ parent.
]

{ #category : #accessing }
RG2Definition >> parent: aRG2BehaviorDefinition [

	"If possible, do not use this method directly."

	parent := aRG2BehaviorDefinition.
	"self addoptToParentStub."

	
]

{ #category : #'as yet unclassified' }
RG2Definition >> postCopy [

	super postCopy.
	
	properties := properties copy.
]

{ #category : #properties }
RG2Definition >> properties [

	^ properties ifNil: [ self cleanedProperties ]
]

{ #category : #properties }
RG2Definition >> propertyNamed: propertyName [ 
	"Answer the value of the annotation named <annotationName>, or 
	nil if the annotation is not defined for the receiver."
	
	^ self
		propertyNamed: propertyName
		ifAbsent: [ nil ]
]

{ #category : #properties }
RG2Definition >> propertyNamed: annotationName ifAbsent: exceptionBlock [ 
	"Answer the value of the property named <annotationName>. If the 
	property is not defined for the receiver, answer the result of 
	evaluating the <exceptionBlock>."

	self hasProperties
		ifFalse: [ ^ exceptionBlock value ].

	^self properties
			at: annotationName
			ifAbsent: [ exceptionBlock value ]
]

{ #category : #properties }
RG2Definition >> propertyNamed: annotationName ifAbsentPut: blockValue [ 

	^self properties
		at: annotationName
		ifAbsentPut: blockValue
]

{ #category : #properties }
RG2Definition >> propertyNamed: annotationName put: value [ 

	self properties
		at: annotationName
		put: value
]

{ #category : #properties }
RG2Definition >> propertyNames [
	
	self hasProperties
		ifFalse:[ ^OrderedCollection new ].
	^ properties keys
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtFullyResolved [

	^ (self pvtResolvableProperties collect: #value) allSatisfy: #isRing2FullyResolved
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtFullyUnresolved [		
		
	^ (self pvtResolvableProperties collect: #value) noneSatisfy: #isRing2FullyResolved 
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtName [

	^ name
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtName: aString [

	name := aString
]

{ #category : #private }
RG2Definition >> pvtParent: aRG2Definition [

	parent := aRG2Definition.
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtResolvableProperties [

	^ { #name -> name }.
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtResolvedProperties [

	^ self pvtResolvableProperties select: [:each | each value isRing2Resolved ] thenCollect: #key
]

{ #category : #'private - backend interface' }
RG2Definition >> pvtUnresolvedProperties [

	^ self pvtResolvableProperties select: [:each | each value isRing2Resolved not ] thenCollect: #key
]

{ #category : #properties }
RG2Definition >> removePropertyNamed: propertyName [ 
	"Remove the annotation named <annotationName>. Fails quietly if there 
	is no annotation with the given name."
	
	(self hasPropertyNamed: propertyName)
		ifTrue:[ properties removeKey: propertyName ]
]

{ #category : #resolving }
RG2Definition >> resolveBehaviorInformation [

	"Resolve basic information about this definition related to the behavior domain (information needed for full class definition etc.). 
	
	We need to avoid cycles so we store information about resolving into a property named #behaviorInformationResolved."
	
	self propertyNamed: #behaviorInformationResolved put: true.

]

{ #category : #resolving }
RG2Definition >> resolveName [

	name := self name.
]

{ #category : #resolving }
RG2Definition >> resolvedProperties [

	^ (self backend resolvedPropertiesFor: self)
]

{ #category : #'as yet unclassified' }
RG2Definition >> timeForStamp: stamp [

	^ stamp
		ifNil: [ 	DateAndTime new]
		ifNotNil: [ 
			| substrings |
			substrings := stamp substrings.
			([(DateAndTime fromString: (' ' join: substrings allButFirst ))] on: Exception do: [DateAndTime epoch]) ]. 
	
]

{ #category : #'as yet unclassified' }
RG2Definition >> unresolvedName [

	^ #unresolved
]

{ #category : #resolving }
RG2Definition >> unresolvedProperties [

	^ (self backend unresolvedPropertiesFor: self)
]

{ #category : #converting }
RG2Definition >> unresolvedValue [

	^ RG2UnresolvedValue new.
]

{ #category : #converting }
RG2Definition >> unresolvedValue: aDefaultValue [

	^ RG2UnresolvedValue new
		default: aDefaultValue;
		yourself
]

{ #category : #children }
RG2Definition >> withChildrenDo: aBlock [

	self withChildrenDo: aBlock traversed: IdentitySet new.
]

{ #category : #children }
RG2Definition >> withChildrenDo: aBlock traversed: aSet [

	(aSet includes: self) ifTrue: [ ^ self ].

	aBlock value: self.
	aSet add: self.
	self childDefinitionsDo: aBlock traversed: aSet.
]
