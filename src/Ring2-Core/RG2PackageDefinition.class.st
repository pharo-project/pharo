"
RG2Package is the representation for packages.
A package manages classes, methods and children packages as elements.
A package can also know in which package is defined

"
Class {
	#name : #RG2PackageDefinition,
	#superclass : #RG2Definition,
	#instVars : [
		'definedBehaviors',
		'extensionMethods',
		'tagsForClasses'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'accessing - backend' }
RG2PackageDefinition >> addClassTag: sSymbol [

	self backend forPackage addClassTag: sSymbol to: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> addDefinedBehavior: aRG2BehaviorDefinition [

	self backend forPackage addDefinedBehavior: aRG2BehaviorDefinition to: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> addExtensionMethod: aRG2MethodDefinition [ 

	self backend forPackage addExtensionMethod: aRG2MethodDefinition to: self
]

{ #category : #'managing container' }
RG2PackageDefinition >> addoptToParentStub [

	self isRing2Resolved ifFalse: [ ^ self ].
	
	super addoptToParentStub.

	^ self parent addPackage: self.
]

{ #category : #'accessing - model' }
RG2PackageDefinition >> asNautilusSelection [

	^ (RPackageOrganizer default packageNamed: self name) asNautilusSelection
]

{ #category : #accessing }
RG2PackageDefinition >> categories [
	
	^ self definedBehaviors collect: #category as: Set

]

{ #category : #accessing }
RG2PackageDefinition >> categoryName [

	^ self name 
]

{ #category : #accessing }
RG2PackageDefinition >> classNames [

	^ self classes collect: #name
]

{ #category : #accessing }
RG2PackageDefinition >> classes [
	"Retrieves a collection of classes defined in the receiver and classes holding extension methods"
		
	^self definedClassesOnly, self extendedClasses 
]

{ #category : #'accessing - model' }
RG2PackageDefinition >> classesTaggedWith: aSymbol [

	^ self definedBehaviors select: [ :each | each isTaggedWith: aSymbol ]
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> cleanDefinedBehaviors [

	self backend forPackage cleanDefinedBehaviorsFor: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> cleanExtensionMethods [

	self backend forPackage cleanExtensionMethodsFor: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> cleanTagsForClasses [

	self backend forPackage cleanTagsForClassesFor: self
]

{ #category : #'as yet unclassified' }
RG2PackageDefinition >> correspondingObjectIn: aRG2Environment [		
		
	^ aRG2Environment ask packageNamed: self name.  
]

{ #category : #'default model values' }
RG2PackageDefinition >> defaultDefinedBehaviors [

	^ Set new
]

{ #category : #'default model values' }
RG2PackageDefinition >> defaultExtensionMethods [

	^ Set new
]

{ #category : #'managing container' }
RG2PackageDefinition >> defaultParentStub [

	^ RG2Environment new
		yourself.
]

{ #category : #'default model values' }
RG2PackageDefinition >> defaultTagsForClasses [

	^ Set new
]

{ #category : #'queries - behaviors' }
RG2PackageDefinition >> definedBehaviors [

	| allDefinedBehaviors |
	allDefinedBehaviors := IdentitySet new.
	self definedBehaviorsDo: [ :each | allDefinedBehaviors add: each].
	^ allDefinedBehaviors asArray
	
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> definedBehaviorsDo: aBlock [

	self backend forPackage definedBehaviorsFor: self do: aBlock
]

{ #category : #accessing }
RG2PackageDefinition >> definedClassNames [

	^ self definedClassesOnly collect: #name 
]

{ #category : #accessing }
RG2PackageDefinition >> definedClasses [

	^ self definedBehaviors
]

{ #category : #'as yet unclassified' }
RG2PackageDefinition >> definedClassesOnly [

	^ self definedBehaviors select: #isClass
]

{ #category : #accessing }
RG2PackageDefinition >> definedTraits [
	"Retrieves the traits defined in the receiver"

	^self definedBehaviors select: #isTrait
]

{ #category : #accessing }
RG2PackageDefinition >> extendedBehaviors [
	
	^ ((self extensionMethods collect: #parent) collect: [:behavior |
		behavior isClass 
			ifTrue: [behavior ]
			ifFalse: [ behavior baseClass]] as: IdentitySet) asArray
]

{ #category : #accessing }
RG2PackageDefinition >> extendedClasses [
	
	^ self extendedBehaviors
]

{ #category : #accessing }
RG2PackageDefinition >> extendedClassesAndTraits [
	
	^ self extendedBehaviors
]

{ #category : #accessing }
RG2PackageDefinition >> extendedSelectors [
	"Retrieves the names of the methods"
	
	^ self extensionMethods collect: #selector
]

{ #category : #accessing }
RG2PackageDefinition >> extendedTraits [
	
	^ (self extensionMethods collect: #parent) select: #isTrait
]

{ #category : #accessing }
RG2PackageDefinition >> extensionMethods [

	| allMethods |
	allMethods := IdentitySet new.
	self extensionMethodsDo: [ :each | allMethods add: each].
	^ allMethods asArray

]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> extensionMethodsDo: aBlock [

	self backend forPackage extensionMethodsFor: self do: aBlock
]

{ #category : #accessing }
RG2PackageDefinition >> findSuitableParentIn: aRG2Environment [

	^ aRG2Environment 
]

{ #category : #initialization }
RG2PackageDefinition >> initialize [

	super initialize.

	extensionMethods := self unresolvedValue: self defaultDefinedBehaviors.
	definedBehaviors := self unresolvedValue: self defaultExtensionMethods.
	tagsForClasses := self unresolvedValue: self defaultTagsForClasses.

]

{ #category : #initialization }
RG2PackageDefinition >> initializeUnresolved [

	super initializeUnresolved.

	extensionMethods := self unresolvedValue: self defaultDefinedBehaviors.
	definedBehaviors := self unresolvedValue: self defaultExtensionMethods.
	tagsForClasses := self unresolvedValue: self defaultTagsForClasses.

]

{ #category : #testing }
RG2PackageDefinition >> isPackage [

	^true
]

{ #category : #resolving }
RG2PackageDefinition >> makeResolved [

	super makeResolved.
	
	definedBehaviors := self definedBehaviors.
	extensionMethods := self extensionMethods.
	tagsForClasses := self tagsForClasses.

]

{ #category : #merging }
RG2PackageDefinition >> merge: aRG2Package [

	super merge: aRG2Package.
	
	((self hasUnresolved: #definedBehaviors) and: [ (aRG2Package hasUnresolved: #definedBehaviors) ])
		ifFalse: [ 
			(self hasResolved: #definedBehaviors) ifFalse: [ 
				self cleanDefinedBehaviors ].
			aRG2Package definedBehaviorsDo: [ :behavior |
				| newDefinition |
				 "create empty unresolved definition of the same kind in the same environment"
				newDefinition := behavior class unresolvedWithParent: self.
				newDefinition merge: behavior.
				self addDefinedBehavior: newDefinition ]].
	
	"TODO:extension methods"
]

{ #category : #accessing }
RG2PackageDefinition >> methods [
	"Retrieves all the methods defined in the receiver. 
	#methods holds the methods of defined classes"
	
	^ (self definedBehaviors flatCollect: #methods), self extensionMethods 
]

{ #category : #accessing }
RG2PackageDefinition >> name: aString [

	| oldName |
	
	oldName := self name.
	super name: aString.
	
	self announce: (RPackageRenamed 
			to: self 
			oldName: oldName 
			newName: aString)
]

{ #category : #'as yet unclassified' }
RG2PackageDefinition >> package [

	^ self
]

{ #category : #printing }
RG2PackageDefinition >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ')'
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtAddClassTag: aSymbol [
	
	tagsForClasses isRing2Resolved ifFalse: [ 
		self pvtCleanTagsForClasses  ].
	
	tagsForClasses add: aSymbol.	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtAddDefinedBehavior: aRG2BehaviorDefinition [

	self environment verifyOwnership: aRG2BehaviorDefinition.
	
	definedBehaviors isRing2Resolved ifFalse: [ 
		self pvtCleanDefinedBehaviors  ].
	
	definedBehaviors add: aRG2BehaviorDefinition.	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtAddExtensionMethod: aMethodDefinition [

	self environment verifyOwnership: aMethodDefinition.
	
	extensionMethods isRing2Resolved ifFalse: [ 
		self pvtCleanExtensionMethods  ].
	
	extensionMethods add: aMethodDefinition.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtCleanDefinedBehaviors [

	definedBehaviors := self defaultDefinedBehaviors.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtCleanExtensionMethods [

	extensionMethods := self defaultExtensionMethods.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtCleanTagsForClasses [

	tagsForClasses := self defaultTagsForClasses.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtDefinedBehaviorsDo: aBlock [

	^ definedBehaviors value do: aBlock
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtExtensionMethodsDo: aBlock [

	^ extensionMethods value do: aBlock
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtRemoveClassTag: aSymbol [
	
	tagsForClasses remove: aSymbol.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtRemoveDefinedBehavior: aRG2BehaviorDefinition [

	self environment verifyOwnership: aRG2BehaviorDefinition.
	
	definedBehaviors remove: aRG2BehaviorDefinition.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtRemoveExtensionMethod: aRG2MethodDefinition [

	self environment verifyOwnership: aRG2MethodDefinition.
	
	extensionMethods remove: aRG2MethodDefinition.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#definedBehaviors -> definedBehaviors. 
		#extensionMethods -> extensionMethods. 
		#tagsForClasses -> tagsForClasses
	}
	
	
]

{ #category : #'private - backend interface' }
RG2PackageDefinition >> pvtTagsForClassesDo: aBlock [

	^ tagsForClasses value do: aBlock
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> removeClassTag: aSymbol [

	self backend forPackage removeClassTag: aSymbol from: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> removeDefinedBehavior: aRG2BehaviorDefinition [

	self backend forPackage removeDefinedBehavior: aRG2BehaviorDefinition from: self
]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> removeExtensionMethod: aRG2MethodDefinition [ 

	self backend forPackage removeExtensionMethod: aRG2MethodDefinition from: self
]

{ #category : #'accessing - model' }
RG2PackageDefinition >> tagsForClasses [
	"Retrieves the traits defined in the receiver"

	| allTags |
	allTags := IdentitySet new.
	self tagsForClassesDo: [ :each | allTags add: each].
	^ allTags asArray

]

{ #category : #'accessing - backend' }
RG2PackageDefinition >> tagsForClassesDo: aBlock [

	self backend forPackage tagsForClassesFor: self do: aBlock
]

{ #category : #accessing }
RG2PackageDefinition >> traits [
	"Retrieves a collection of classes defined in the receiver and classes holding extension methods"
		
	^self definedTraits, self extendedTraits
]
