"
I answer a boolean that indicates if a EpCodeChange would actually produce a change in current image if applied with EpApplyVisitor.
"
Class {
	#name : 'EpHasImpactVisitor',
	#superclass : 'Object',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#instVars : [
		'environment'
	],
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'instance creation' }
EpHasImpactVisitor class >> inEnvironment: anEnvironment [

	^ self basicNew
		initializeWith: anEnvironment;
		yourself
]

{ #category : 'private' }
EpHasImpactVisitor >> behaviorNamed: aSymbol ifPresent: presentBlock [

	^ (environment classOrTraitNamed: aSymbol)
		ifNil: [ self ]
		ifNotNil: presentBlock
]

{ #category : 'initialization' }
EpHasImpactVisitor >> initializeWith: anEnvironment [

	self initialize.
	environment := anEnvironment
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitBehaviorChange: aChange [
	self behaviorNamed: aChange behaviorAffectedName ifPresent: [ :behavior |
		^ behavior definitionString ~= aChange behaviorAffected definitionSource or: [
			behavior classSide definitionString ~= aChange behaviorAffected classSide definitionSource ] ].

	^ true
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitBehaviorCommentChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass |
			^ aClass comment ~= aChange newComment or: [
			aClass commentStamp ~= aChange newStamp ] ].

	^ true
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitBehaviorNameChange: aChange [

	self
		behaviorNamed: aChange oldName
		ifPresent: [ :behavior | ^ true ].

	^ false
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [

	| classRepackaged |
	classRepackaged := aBehaviorRepackagedChange behaviorAffected.
	self
		behaviorNamed: classRepackaged name
		ifPresent: [ :behavior | ^ (behavior package name = classRepackaged package name and: [ behavior packageTag name = classRepackaged packageTag ]) not ].

	^ true
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitClassRemoval: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ true ].

	^ false
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitCodeChange: aChange [
	^ self error: 'shouldn''t happen'
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitMethodChange: aChange [
	"Addition and Modification come here"

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^ behavior
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method |
					method protocol name ~= aChange methodAffectedProtocol or: [
					method sourceCode ~= aChange methodAffectedSourceCode ] ]
				ifAbsent: [ true ] ].

	^ true "If class is not present"
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitMethodRemoval: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			"If method comes from trait, removing has no impact."
			^ behavior
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method | method isFromTrait not ]
				ifAbsent: [ false ] ].

	^ false "No impact if container does not exist"
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageAddition: aPackageAddition [

	^ (self packageOrganizer hasPackage: aPackageAddition packageName) not
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageRemoval: aPackageRemoval [

	^ self packageOrganizer hasPackage: aPackageRemoval packageName
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageRename: aPackageRename [

	^ environment organization hasPackage: aPackageRename oldPackageName
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageTagAddition: aPackageTagAddition [

	^ (environment organization hasTag: aPackageTagAddition tagName inPackage: aPackageTagAddition packageName) not
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageTagRemoval: aPackageTagRemoval [

	^ environment organization hasTag: aPackageTagRemoval tagName inPackage: aPackageTagRemoval packageName
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitPackageTagRename: aPackageTagRename [

	^ environment organization hasTag: aPackageTagRename oldTagName inPackage: aPackageTagRename packageName
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitProtocolAddition: aProtocolAddition [

	self
		behaviorNamed: aProtocolAddition behaviorAffectedName
		ifPresent: [ :behavior | ^ (behavior hasProtocol: aProtocolAddition protocol) not ].

	^ true
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitProtocolRemoval: aProtocolRemoved [
	"Protocol removal has an impact if:
	- the protocl exists, and
	- there are methods in the protocol that are not trait methods"

	self behaviorNamed: aProtocolRemoved behaviorAffectedName ifPresent: [ :behavior |
		(behavior hasProtocol: aProtocolRemoved protocol) ifFalse: [ ^ false ].
		((behavior methodsInProtocol: aProtocolRemoved protocol) allSatisfy: [ :method | method isFromTrait ]) ifTrue: [ ^ false ] ].

	^ true
]

{ #category : 'visitor' }
EpHasImpactVisitor >> visitTraitRemoval: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ true ].

	^ false
]
