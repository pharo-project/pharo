"
RGPackage is the representation for packages.
A package manages classes, methods and children packages as elements.
A package can also know in which package is defined

"
Class {
	#name : 'RGPackage',
	#superclass : 'RGObject',
	#instVars : [
		'definedBehaviors',
		'extensionMethods',
		'tagsForClasses'
	],
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'accessing - backend' }
RGPackage >> addClassTag: sSymbol [

	self backend forPackage addClassTag: sSymbol to: self
]

{ #category : 'accessing - backend' }
RGPackage >> addDefinedBehavior: anRGBehavior [

	self backend forPackage addDefinedBehavior: anRGBehavior to: self
]

{ #category : 'accessing - backend' }
RGPackage >> addExtensionMethod: anRGMethod [

	self backend forPackage addExtensionMethod: anRGMethod to: self
]

{ #category : 'managing container' }
RGPackage >> addoptToParentStub [

	self isRingResolved ifFalse: [ ^ self ].

	super addoptToParentStub.

	^ self parent addPackage: self
]

{ #category : 'accessing' }
RGPackage >> calypsoName [

	^ self name
]

{ #category : 'accessing' }
RGPackage >> categories [

	^ self definedBehaviors collect: #category as: Set
]

{ #category : 'accessing' }
RGPackage >> categoryName [

	^ self name
]

{ #category : 'accessing' }
RGPackage >> classNames [

	^ self classes collect: [ :each | each name ]
]

{ #category : 'accessing' }
RGPackage >> classes [
	"Retrieves a collection of classes defined in the receiver and classes holding extension methods"

	^self definedClassesOnly, self extendedClasses
]

{ #category : 'accessing - model' }
RGPackage >> classesTaggedWith: aSymbol [

	^ self definedBehaviors select: [ :each | each isTaggedWith: aSymbol ]
]

{ #category : 'accessing - backend' }
RGPackage >> cleanDefinedBehaviors [

	self backend forPackage cleanDefinedBehaviorsFor: self
]

{ #category : 'accessing - backend' }
RGPackage >> cleanExtensionMethods [

	self backend forPackage cleanExtensionMethodsFor: self
]

{ #category : 'accessing - backend' }
RGPackage >> cleanTagsForClasses [

	self backend forPackage cleanTagsForClassesFor: self
]

{ #category : 'default model values' }
RGPackage >> defaultDefinedBehaviors [

	^ Set new
]

{ #category : 'default model values' }
RGPackage >> defaultExtensionMethods [

	^ Set new
]

{ #category : 'managing container' }
RGPackage >> defaultParentStub [

	^ RGEnvironment new
		yourself
]

{ #category : 'default model values' }
RGPackage >> defaultTagsForClasses [

	^ Set new
]

{ #category : 'queries - behaviors' }
RGPackage >> definedBehaviors [

	| allDefinedBehaviors |
	allDefinedBehaviors := IdentitySet new.
	self definedBehaviorsDo: [ :each | allDefinedBehaviors add: each].
	^ allDefinedBehaviors asArray
]

{ #category : 'accessing - backend' }
RGPackage >> definedBehaviorsDo: aBlock [

	self backend forPackage definedBehaviorsFor: self do: aBlock
]

{ #category : 'accessing' }
RGPackage >> definedClassNames [

	^ self definedClassesOnly collect: [ :each | each name ]
]

{ #category : 'accessing' }
RGPackage >> definedClasses [

	^ self definedBehaviors
]

{ #category : 'queries - behaviors' }
RGPackage >> definedClassesOnly [

	^ self definedBehaviors select: #isClass
]

{ #category : 'accessing' }
RGPackage >> definedTraits [
	"Retrieves the traits defined in the receiver"

	^self definedBehaviors select: #isTrait
]

{ #category : 'accessing' }
RGPackage >> definitionFor: aRGClassStrategy [
	"Answer a <String>. Use aRGBehaviourStrategy to access the definition <String> for the receiver"

	^ aRGClassStrategy definitionStringFor: self
]

{ #category : 'accessing' }
RGPackage >> extendedBehaviors [

	^ ((self extensionMethods collect: [:each | each parent]) collect: [:behavior |
		behavior isClass
			ifTrue: [behavior ]
			ifFalse: [ behavior baseClass]] as: IdentitySet) asArray
]

{ #category : 'accessing' }
RGPackage >> extendedClasses [

	^ self extendedBehaviors
]

{ #category : 'accessing' }
RGPackage >> extendedClassesAndTraits [

	^ self extendedBehaviors
]

{ #category : 'accessing' }
RGPackage >> extendedSelectors [
	"Retrieves the names of the methods"

	^ self extensionMethods collect: [ :each | each selector ]
]

{ #category : 'accessing' }
RGPackage >> extendedTraits [

	^ (self extensionMethods collect: [:each | each parent]) select: [:each | each isTrait]
]

{ #category : 'accessing' }
RGPackage >> extensionMethods [

	| allMethods |
	allMethods := IdentitySet new.
	self extensionMethodsDo: [ :each | allMethods add: each].
	^ allMethods asArray
]

{ #category : 'accessing - backend' }
RGPackage >> extensionMethodsDo: aBlock [

	self backend forPackage extensionMethodsFor: self do: aBlock
]

{ #category : 'initialization' }
RGPackage >> initialize [

	super initialize.

	extensionMethods := self unresolvedValue: self defaultExtensionMethods.
	definedBehaviors := self unresolvedValue: self defaultDefinedBehaviors.
	tagsForClasses := self unresolvedValue: self defaultTagsForClasses
]

{ #category : 'initialization' }
RGPackage >> initializeUnresolved [

	super initializeUnresolved.

	extensionMethods := self unresolvedValue: self defaultDefinedBehaviors.
	definedBehaviors := self unresolvedValue: self defaultExtensionMethods.
	tagsForClasses := self unresolvedValue: self defaultTagsForClasses
]

{ #category : 'testing' }
RGPackage >> isPackage [

	^true
]

{ #category : 'resolving' }
RGPackage >> makeResolved [

	super makeResolved.

	definedBehaviors := self definedBehaviors markAsRingResolved.
	extensionMethods := self extensionMethods markAsRingResolved.
	tagsForClasses := self tagsForClasses markAsRingResolved
]

{ #category : 'accessing' }
RGPackage >> methods [
	"Retrieves all the methods defined in the receiver.
	#methods holds the methods of defined classes"

	^ (self definedBehaviors flatCollect: #methods), self extensionMethods
]

{ #category : 'accessing' }
RGPackage >> name: aString [

	| oldName |

	oldName := self name.
	super name: aString.

	self announce: (PackageRenamed
			to: self
			oldName: oldName
			newName: aString)
]

{ #category : 'accessing' }
RGPackage >> package [

	^ self
]

{ #category : 'printing' }
RGPackage >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ')'
]

{ #category : 'private - backend interface' }
RGPackage >> pvtAddClassTag: aSymbol [

	tagsForClasses isRingResolved ifFalse: [
		self pvtCleanTagsForClasses  ].

	tagsForClasses add: aSymbol
]

{ #category : 'private - backend interface' }
RGPackage >> pvtAddDefinedBehavior: anRGBehavior [

	self environment verifyOwnership: anRGBehavior.

	definedBehaviors isRingResolved ifFalse: [
		self pvtCleanDefinedBehaviors  ].

	definedBehaviors add: anRGBehavior
]

{ #category : 'private - backend interface' }
RGPackage >> pvtAddExtensionMethod: aMethodDefinition [

	self environment verifyOwnership: aMethodDefinition.

	extensionMethods isRingResolved ifFalse: [
		self pvtCleanExtensionMethods  ].

	extensionMethods add: aMethodDefinition.

	"TODO:Announce"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtCleanDefinedBehaviors [

	definedBehaviors := self defaultDefinedBehaviors.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtCleanExtensionMethods [

	extensionMethods := self defaultExtensionMethods.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtCleanTagsForClasses [

	tagsForClasses := self defaultTagsForClasses.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtDefinedBehaviorsDo: aBlock [

	^ definedBehaviors value do: aBlock
]

{ #category : 'private - backend interface' }
RGPackage >> pvtExtensionMethodsDo: aBlock [

	^ extensionMethods value do: aBlock
]

{ #category : 'private - backend interface' }
RGPackage >> pvtRemoveClassTag: aSymbol [

	tagsForClasses remove: aSymbol.

	"TODO:Announce"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtRemoveDefinedBehavior: anRGBehavior [

	self environment verifyOwnership: anRGBehavior.

	definedBehaviors remove: anRGBehavior.

	"TODO:Announce"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtRemoveExtensionMethod: anRGMethod [

	self environment verifyOwnership: anRGMethod.

	extensionMethods remove: anRGMethod.

	"TODO:Announce"
]

{ #category : 'private - backend interface' }
RGPackage >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#definedBehaviors -> definedBehaviors.
		#extensionMethods -> extensionMethods.
		#tagsForClasses -> tagsForClasses
	}
]

{ #category : 'private - backend interface' }
RGPackage >> pvtTagsForClassesDo: aBlock [

	^ tagsForClasses value do: aBlock
]

{ #category : 'accessing - backend' }
RGPackage >> removeClassTag: aSymbol [

	self backend forPackage removeClassTag: aSymbol from: self
]

{ #category : 'accessing - backend' }
RGPackage >> removeDefinedBehavior: anRGBehavior [

	self backend forPackage removeDefinedBehavior: anRGBehavior from: self
]

{ #category : 'accessing - backend' }
RGPackage >> removeExtensionMethod: anRGMethod [

	self backend forPackage removeExtensionMethod: anRGMethod from: self
]

{ #category : 'accessing - model' }
RGPackage >> tagsForClasses [
	"Retrieves the traits defined in the receiver"

	| allTags |
	allTags := IdentitySet new.
	self tagsForClassesDo: [ :each | allTags add: each].
	^ allTags asArray
]

{ #category : 'accessing - backend' }
RGPackage >> tagsForClassesDo: aBlock [

	self backend forPackage tagsForClassesFor: self do: aBlock
]

{ #category : 'accessing' }
RGPackage >> traits [
	"Retrieves a collection of classes defined in the receiver and classes holding extension methods"

	^self definedTraits, self extendedTraits
]
