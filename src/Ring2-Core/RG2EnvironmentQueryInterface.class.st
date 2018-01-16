Class {
	#name : #RG2EnvironmentQueryInterface,
	#superclass : #Object,
	#instVars : [
		'environment',
		'behaviorsDictionary'
	],
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface class >> for: aRG2Environment [

	^ self new 
		environment: aRG2Environment;
		yourself.
]

{ #category : #accessing }
RG2EnvironmentQueryInterface >> allClasses [

	^ self behaviors select: [ :each | each isClass ] 
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> allClassesAndTraits [

	^ self behaviors select: [ :each | each isClass or: [ each isTrait and: [ each isMetaclassTrait not ] ] ]
]

{ #category : #accessing }
RG2EnvironmentQueryInterface >> behaviorNamed: aSymbol [

	^ self behaviorNamed: aSymbol ifAbsent: [ nil]

]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> behaviorNamed: aSymbol ifAbsent: aBlock [

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
RG2EnvironmentQueryInterface >> behaviorNamedExactlyAs: aSymbol [ 

	^ self behaviorNamedExactlyAs: aSymbol ifAbsent: [nil]
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> behaviorNamedExactlyAs: aSymbol ifAbsent: aBlock [

	^ self behaviorsDictionary at: aSymbol asSymbol ifAbsent: aBlock
	"	
	self behaviorsDo: [:each |
		(each name = aSymbol) ifTrue: [ ^ each ] ].
	
	^ nil
	"
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> behaviors [
	"Retrieves the traits defined in the receiver"

	| allBehaviors |
	allBehaviors := IdentitySet new.
	self behaviorsDo: [ :each | allBehaviors add: each].
	^ allBehaviors asArray

]

{ #category : #caching }
RG2EnvironmentQueryInterface >> behaviorsDictionary [

	^ behaviorsDictionary ifNil: [ 
		behaviorsDictionary := IdentityDictionary new.
		self behaviorsDo: [:each |
			behaviorsDictionary at: each name put: each ]. 
		behaviorsDictionary
		]
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> behaviorsDo: aBlock [

	self environment behaviorsDo: aBlock
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> classNamed: aSymbol [

	^ self behaviorNamed: aSymbol
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> classOrTraitNamed: aSymbol [
	
	^ self behaviorNamed: aSymbol
]

{ #category : #accessing }
RG2EnvironmentQueryInterface >> environment [
	^ environment
]

{ #category : #accessing }
RG2EnvironmentQueryInterface >> environment: anObject [
	environment := anObject
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> globalVariables [
	"Retrieves the traits defined in the receiver"

	| allGlobalVariables |
	allGlobalVariables := IdentitySet new.
	self globalVariablesDo: [ :each | allGlobalVariables add: each].
	^ allGlobalVariables asArray

]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> globalVariablesDo: aBlock [

	self environment globalVariablesDo: aBlock
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> includesClassNamed: aSymbol [ 

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
RG2EnvironmentQueryInterface >> invalidateName: aSymbol [

	behaviorsDictionary removeKey: aSymbol
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> packageNamed: aString [
	
	self packagesDo: [:each |
		(each name = aString) ifTrue: [ ^ each ] ].
	
	^ nil
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> packages [
	"Retrieves the traits defined in the receiver"

	| allPackages |
	allPackages := IdentitySet new.
	self packagesDo: [ :each | allPackages add: each].
	^ allPackages asArray

]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> packagesDo: aBlock [

	self environment packagesDo: aBlock
]

{ #category : #caching }
RG2EnvironmentQueryInterface >> replaceName: aSymbol with: aNewSymbol [

	| aRG2Behavior |
	aRG2Behavior := behaviorsDictionary at: aSymbol ifAbsent: [ nil ].
	aRG2Behavior ifNotNil: [ 
		behaviorsDictionary removeKey: aSymbol.
		behaviorsDictionary at: aNewSymbol put: aRG2Behavior.
		 ]
	
]

{ #category : #caching }
RG2EnvironmentQueryInterface >> resetBehaviorsDictionary [

	behaviorsDictionary := IdentityDictionary new
]

{ #category : #'as yet unclassified' }
RG2EnvironmentQueryInterface >> traitNamed: traitName [
	"Retrieves an RG2TraitDefinition object. The traitName could be theMetaClass name"
	| trait |
	
	^(trait:= self classOrTraitNamed: traitName) isTrait
		ifTrue:[ trait ]
		ifFalse:[ nil ]
]
