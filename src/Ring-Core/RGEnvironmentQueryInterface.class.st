Class {
	#name : #RGEnvironmentQueryInterface,
	#superclass : #Object,
	#instVars : [
		'environment',
		'behaviorsDictionary'
	],
	#category : #'Ring-Core-Environment'
}

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface class >> for: anRGEnvironment [

	^ self new 
		environment: anRGEnvironment;
		yourself.
]

{ #category : #accessing }
RGEnvironmentQueryInterface >> allClasses [

	^ self behaviors select: [ :each | each isClass ] 
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> allClassesAndTraits [

	^ self behaviors select: [ :each | each isClass or: [ each isTrait and: [ each isMetaclassTrait not ] ] ]
]

{ #category : #accessing }
RGEnvironmentQueryInterface >> behaviorNamed: aSymbol [

	^ self behaviorNamed: aSymbol ifAbsent: [ nil]

]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> behaviorNamed: aSymbol ifAbsent: aBlock [

	| found siblingName | 
	
	found := self behaviorNamedExactlyAs: aSymbol ifAbsent: [nil].
	found ifNotNil: [ ^ found ].
	
	(aSymbol endsWith: ' class') 
		ifTrue: [ 
			siblingName := (aSymbol withoutSuffix: ' class').
			found := self behaviorNamedExactlyAs: siblingName ifAbsent: [nil].
			found ifNotNil: [ 
				found isClass ifTrue: [ ^ found metaclass ]]].

	(aSymbol endsWith: ' classTrait') 
		ifTrue: [ 
			siblingName := (aSymbol withoutSuffix: ' classTrait').
			found := self behaviorNamedExactlyAs: siblingName ifAbsent: [nil].
			found ifNotNil: [ 
				found isTrait ifTrue: [ ^ found classTrait ]]].
		
	siblingName := (aSymbol, ' class').
	found := self behaviorNamedExactlyAs: siblingName ifAbsent: [nil].
	found ifNotNil: [ ^ found baseClass ].		

	siblingName := (aSymbol, ' classTrait').
	found := self behaviorNamedExactlyAs: siblingName ifAbsent: [nil].
	found ifNotNil: [ ^ found baseTrait ].		
					
	^ aBlock value.
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> behaviorNamedExactlyAs: aSymbol [ 

	^ self behaviorNamedExactlyAs: aSymbol ifAbsent: [nil]
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> behaviorNamedExactlyAs: aSymbol ifAbsent: aBlock [

	^ self behaviorsDictionary at: aSymbol asSymbol ifAbsent: aBlock
	"	
	self behaviorsDo: [:each |
		(each name = aSymbol) ifTrue: [ ^ each ] ].
	
	^ nil
	"
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> behaviors [
	"Retrieves the traits defined in the receiver"

	| allBehaviors |
	allBehaviors := IdentitySet new.
	self behaviorsDo: [ :each | allBehaviors add: each].
	^ allBehaviors asArray

]

{ #category : #caching }
RGEnvironmentQueryInterface >> behaviorsDictionary [

	^ behaviorsDictionary ifNil: [ 
		behaviorsDictionary := IdentityDictionary new.
		self behaviorsDo: [:each |
			behaviorsDictionary at: each name put: each ]. 
		behaviorsDictionary
		]
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> behaviorsDo: aBlock [

	self environment behaviorsDo: aBlock
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> classNamed: aSymbol [

	^ self behaviorNamed: aSymbol
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> classOrTraitNamed: aSymbol [
	
	^ self behaviorNamed: aSymbol
]

{ #category : #accessing }
RGEnvironmentQueryInterface >> environment [
	^ environment
]

{ #category : #accessing }
RGEnvironmentQueryInterface >> environment: anObject [
	environment := anObject
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> globalVariables [
	"Retrieves the traits defined in the receiver"

	| allGlobalVariables |
	allGlobalVariables := IdentitySet new.
	self globalVariablesDo: [ :each | allGlobalVariables add: each].
	^ allGlobalVariables asArray

]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> globalVariablesDo: aBlock [

	self environment globalVariablesDo: aBlock
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> includesClassNamed: aSymbol [ 

	self globalVariablesDo: [ :var |
		var name = aSymbol ifTrue: [ ^ true ] ].

	^ self behaviorsDictionary 
		at: aSymbol asSymbol 
		ifPresent: [ :element | true ]
		ifAbsent: [ false ]
	"	
	self behaviorsDo: [:each |
		(each name = aSymbol) ifTrue: [ ^ each ] ].
	
	^ nil
	"
]

{ #category : #caching }
RGEnvironmentQueryInterface >> invalidateName: aSymbol [

	behaviorsDictionary removeKey: aSymbol
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> packageNamed: aString [
	
	self packagesDo: [:each |
		(each name = aString) ifTrue: [ ^ each ] ].
	
	^ nil
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> packages [
	"Retrieves the traits defined in the receiver"

	| allPackages |
	allPackages := IdentitySet new.
	self packagesDo: [ :each | allPackages add: each].
	^ allPackages asArray

]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> packagesDo: aBlock [

	self environment packagesDo: aBlock
]

{ #category : #caching }
RGEnvironmentQueryInterface >> replaceName: aSymbol with: aNewSymbol [

	| anRGBehavior |
	anRGBehavior := behaviorsDictionary at: aSymbol ifAbsent: [ nil ].
	anRGBehavior ifNotNil: [ 
		behaviorsDictionary removeKey: aSymbol.
		behaviorsDictionary at: aNewSymbol put: anRGBehavior.
		 ]
	
]

{ #category : #caching }
RGEnvironmentQueryInterface >> resetBehaviorsDictionary [

	behaviorsDictionary := IdentityDictionary new
]

{ #category : #'as yet unclassified' }
RGEnvironmentQueryInterface >> traitNamed: traitName [
	"Retrieves an RGTrait object. The traitName could be theMetaClass name"
	| trait |
	
	^(trait:= self classOrTraitNamed: traitName) isTrait
		ifTrue:[ trait ]
		ifFalse:[ nil ]
]
