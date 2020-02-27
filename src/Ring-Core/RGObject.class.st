"
I am the root class of the Ring meta-model definitions.
A Ring definition has a name, properties and knows its environment
"
Class {
	#name : #RGObject,
	#superclass : #Object,
	#instVars : [
		'properties',
		'name',
		'parent'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGObject class >> asYetUnclassifiedProtocolName [

	^ 'as yet unclassified' asSymbol
]

{ #category : #'instance creation' }
RGObject class >> named: aString [

	"create model in its own environment"

	^self new
		pvtName: aString;
		yourself
]

{ #category : #'instance creation' }
RGObject class >> named: aName parent: anRGObject [

	^self basicNew
		parent: anRGObject;
		initialize;
		pvtName: aName asSymbol;
		yourself
]

{ #category : #'instance creation' }
RGObject class >> parent: anRGObject [

	^self basicNew
		parent: anRGObject;
		initialize;
		yourself
]

{ #category : #'instance creation' }
RGObject class >> unnamed [

	^self new
]

{ #category : #'instance creation' }
RGObject class >> unresolved [

	^ self basicNew
		initializeUnresolved;
		yourself.
]

{ #category : #'instance creation' }
RGObject class >> unresolvedNamed: aString withParent: anRGDefintion [

	^self basicNew
		initializeUnresolved; 
		pvtName: aString asSymbol;
		parent: anRGDefintion;
		yourself
]

{ #category : #'instance creation' }
RGObject class >> unresolvedWithParent: anRGDefintion [

	^self basicNew
		initializeUnresolved; 
		parent: anRGDefintion;
		yourself
]

{ #category : #'managing container' }
RGObject >> addoptToParentStub [
]

{ #category : #announcements }
RGObject >> announce: anAnnouncement [

	self environment announcer announce: anAnnouncement
]

{ #category : #announcements }
RGObject >> announcer [

	^ self environment announcer
]

{ #category : #converting }
RGObject >> asRGDefinition [

	^ self
]

{ #category : #accessing }
RGObject >> ask [

	^ self 
]

{ #category : #accessing }
RGObject >> backend [
	
	^ self environment backend
]

{ #category : #events }
RGObject >> child: aChildDefinition renamedFrom: oldName to: newName [

	"react on renaming of a child definition. By default do nothing"
]

{ #category : #copying }
RGObject >> copyForBehaviorDefinition [

	"returns copy of the behavior model that contains only information related to the behavior 
	definition"
	<ignoreForMutations> "removing of ^ can cause infinite loops"
	^ self shallowCopy copyForBehaviorDefinitionPostCopy
]

{ #category : #copying }
RGObject >> copyForBehaviorDefinitionPostCopy [

	<ignoreForMutations>

	super postCopy.
	
	
	
	
]

{ #category : #properties }
RGObject >> copyPropertiesFrom: anRGObject [

	properties := anRGObject properties copy.
]

{ #category : #copying }
RGObject >> copyWithParent: newParent [

	"create copy of the definition and then assign the new parent"
	
	^ self copy 
		parent: newParent;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultClassStub [

	<ignoreForMutations>
	^ RGClass unresolved
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultCommentStubIn: anRGBehavior [

	<ignoreForMutations>
	^ RGComment unresolved
		parent: anRGBehavior;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultEnvironmentStub [

	<ignoreForMutations>
	^ RGEnvironment unresolved
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultFixedLayoutStub [

	<ignoreForMutations>
	^ RGFixedLayout unresolved
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultFixedLayoutStubIn: anRGBehavior [

	<ignoreForMutations>
	^ RGFixedLayout unresolved
		parent: anRGBehavior;
		yourself.
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultPackageStubIn: anRGEnvironment [

	<ignoreForMutations>
	^ RGPackage unresolved
		parent: anRGEnvironment;
		yourself.
]

{ #category : #'managing container' }
RGObject >> defaultParentStub [

	self subclassResponsibility
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultTraitCompositionStub [

	^ RGTraitComposition unresolved
]

{ #category : #'hierarchy-defaults' }
RGObject >> defaultTraitCompositionStubIn: anRGBehavior [
	<ignoreForMutations>

	^ RGTraitComposition unresolved
		parent: anRGBehavior;
		yourself.
]

{ #category : #accessing }
RGObject >> environment [
	
	<ignoreForMutations>

	^ self propertyNamed: #environment ifAbsent: [ self parent environment ].
]

{ #category : #copying }
RGObject >> fullCopy [

	^ self copy.
]

{ #category : #accessing }
RGObject >> fullName [

	^self name
]

{ #category : #properties }
RGObject >> hasProperties [

	^ properties isEmptyOrNil not
]

{ #category : #properties }
RGObject >> hasPropertyNamed: propertyName [
	
	^ self hasProperties
		ifTrue: [ properties includesKey: propertyName ]	
		ifFalse:[ false ].

]

{ #category : #resolving }
RGObject >> hasResolved: propertySymbol [

	^ self resolvedProperties includes: propertySymbol



]

{ #category : #resolving }
RGObject >> hasResolvedAll: aCollectionOfSymbols [

	^ self resolvedProperties includesAll: aCollectionOfSymbols



]

{ #category : #testing }
RGObject >> hasResolvedName [		
	
	^ self hasResolved: #name
]

{ #category : #resolving }
RGObject >> hasUnresolved: propertySymbol [

	^ (self resolvedProperties includes: propertySymbol) not



]

{ #category : #resolving }
RGObject >> hasUnresolvedAll: aCollectionOfSymbols [

	^ (self resolvedProperties includesAll: aCollectionOfSymbols) not



]

{ #category : #initialization }
RGObject >> initialize [

	super initialize.

	name := self unresolvedValue: self unresolvedName.


]

{ #category : #initialization }
RGObject >> initializeUnresolved [

	self propertyNamed: #resolved put: false. 
	name := self unresolvedValue: self unresolvedName.
	


]

{ #category : #'testing types' }
RGObject >> isBehavior [

	^ false
]

{ #category : #'testing types' }
RGObject >> isClass [

	^ false
]

{ #category : #'testing types' }
RGObject >> isComment [

	^false
]

{ #category : #'testing types' }
RGObject >> isEnvironment [

	^false
]

{ #category : #'testing types' }
RGObject >> isGlobalVariable [

	^false
]

{ #category : #'testing types' }
RGObject >> isLayout [

	^false
]

{ #category : #testing }
RGObject >> isMeta [

	^ false
]

{ #category : #'testing types' }
RGObject >> isMetaclass [

	^ false
]

{ #category : #'testing types' }
RGObject >> isMetaclassTrait [

	^ false
]

{ #category : #'testing types' }
RGObject >> isMethod [

	^false
]

{ #category : #'testing types' }
RGObject >> isPackage [

	^false
]

{ #category : #'testing types' }
RGObject >> isProtocol [

	^false
]

{ #category : #testing }
RGObject >> isRGObject [

	^true
]

{ #category : #resolving }
RGObject >> isRingFullyResolved [

	^ self backend hasFullyResolved: self
]

{ #category : #resolving }
RGObject >> isRingFullyUnresolved [		
	
	^ self backend hasFullyUnresolved: self 		


]

{ #category : #testing }
RGObject >> isRingResolved [

	^ self propertyNamed: #resolved ifAbsent: [ true ]
]

{ #category : #'testing types' }
RGObject >> isSlot [

	^ false
]

{ #category : #'testing types' }
RGObject >> isTraitAlias [

	^ false
]

{ #category : #'testing types' }
RGObject >> isTraitComposition [

	^ false
]

{ #category : #'testing types' }
RGObject >> isTraitExclusion [

	^ false
]

{ #category : #'testing types' }
RGObject >> isTraitTransformation [

	^ false
]

{ #category : #'testing types' }
RGObject >> isVariable [

	"is the definition a variable class?"

	^ false
]

{ #category : #'testing types' }
RGObject >> isVariableDefinition [

	^ false
]

{ #category : #resolving }
RGObject >> makeResolved [

	self markAsRingResolved.
	
	name := self name markAsRingResolved.
]

{ #category : #resolving }
RGObject >> markAsRingResolved [

	self propertyNamed: #resolved put: true.
	
]

{ #category : #accessing }
RGObject >> name [

	^ (self backend nameFor: self) orDefaultForUnresolved 
]

{ #category : #accessing }
RGObject >> name: aString [

	| oldName |
	
	oldName := name.
	self backend setNameFor: self to: aString.
	self parent child: self renamedFrom: oldName to: aString.

]

{ #category : #accessing }
RGObject >> package [

	^ self parent package
]

{ #category : #accessing }
RGObject >> parent [
	"The parent of a class definition element: method, comment and variable is the class definition. This method retrieves the class that defines such element"
	
	<ignoreForMutations>
	parent ifNil: [ 
		self parent: self defaultParentStub.
		self addoptToParentStub ].
	
	^ parent.
]

{ #category : #accessing }
RGObject >> parent: anRGBehavior [

	"If possible, do not use this method directly."
	<ignoreForMutations>

	parent := anRGBehavior.
	"self addoptToParentStub."

	
]

{ #category : #copying }
RGObject >> postCopy [

	super postCopy.
	
	properties := properties copy.
]

{ #category : #properties }
RGObject >> properties [

	^ properties ifNil: [ properties := IdentityDictionary new ]
]

{ #category : #properties }
RGObject >> propertyNamed: propertyName [ 
	
	^ self
		propertyNamed: propertyName
		ifAbsent: [ nil ]
]

{ #category : #properties }
RGObject >> propertyNamed: annotationName ifAbsent: exceptionBlock [ 

	^ self properties
			at: annotationName
			ifAbsent: [ exceptionBlock value ]
]

{ #category : #properties }
RGObject >> propertyNamed: annotationName ifAbsentPut: blockValue [ 

	^ self properties
		at: annotationName
		ifAbsentPut: blockValue
]

{ #category : #properties }
RGObject >> propertyNamed: annotationName put: value [ 

	self properties
		at: annotationName
		put: value
]

{ #category : #properties }
RGObject >> propertyNames [
	
	self hasProperties
		ifFalse:[ ^ OrderedCollection new ].
	^ properties keys
]

{ #category : #'private - backend interface' }
RGObject >> pvtFullyResolved [

	^ (self pvtResolvableProperties collect: #value) allSatisfy: #isRingFullyResolved
]

{ #category : #'private - backend interface' }
RGObject >> pvtFullyUnresolved [		
		
	^ (self pvtResolvableProperties collect: #value) noneSatisfy: #isRingFullyResolved 
]

{ #category : #'private - backend interface' }
RGObject >> pvtName [

	^ name
]

{ #category : #'private - backend interface' }
RGObject >> pvtName: aString [

	 name := aString
]

{ #category : #private }
RGObject >> pvtParent: anRGObject [

	parent := anRGObject.
]

{ #category : #'private - backend interface' }
RGObject >> pvtResolvableProperties [

	^ { #name -> name }.
]

{ #category : #'private - backend interface' }
RGObject >> pvtResolvedProperties [

	^ self pvtResolvableProperties select: [:each | each value isRingResolved ] thenCollect: #key
]

{ #category : #'private - backend interface' }
RGObject >> pvtUnresolvedProperties [

	^ self pvtResolvableProperties select: [:each | each value isRingResolved not ] thenCollect: #key
]

{ #category : #properties }
RGObject >> removePropertyNamed: propertyName [ 
	
	(self hasPropertyNamed: propertyName)
		ifTrue: [ properties removeKey: propertyName ]
]

{ #category : #accessing }
RGObject >> resolvedNameOrNil [

	^ self hasResolvedName
		ifTrue: [ self name. ]
		ifFalse: [ nil ].		
]

{ #category : #resolving }
RGObject >> resolvedProperties [

	^ (self backend resolvedPropertiesFor: self)
]

{ #category : #accessing }
RGObject >> unresolveName [

	| oldName result |
	
	oldName := name.
	result := self unresolvedValue: self unresolvedName.
	self pvtName: result.
	self parent child: self renamedFrom: oldName to: result value.
	^ result
]

{ #category : #defaults }
RGObject >> unresolvedName [

	^ #unresolved
]

{ #category : #resolving }
RGObject >> unresolvedProperties [

	^ (self backend unresolvedPropertiesFor: self)
]

{ #category : #converting }
RGObject >> unresolvedValue [

	^ RGUnresolvedValue new.
]

{ #category : #converting }
RGObject >> unresolvedValue: aDefaultValue [

	^ RGUnresolvedValue new
		default: aDefaultValue;
		yourself
]
