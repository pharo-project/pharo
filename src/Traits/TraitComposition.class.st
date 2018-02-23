"
I hold a collection of trait transformations and provide important facilities to query the trait composition. For each trait in the composition clause there exists exactly one transformation in the collection.

Note, that directly manipulating the composition of a class or trait does not effect changes automatically. Use PureBehavior>>setTraitComposition: to do this. You have to make a copy of the old trait composition before changing it because only the difference between the new and the old composition is updated!
"
Class {
	#name : #TraitComposition,
	#superclass : #Object,
	#instVars : [
		'transformations'
	],
	#category : #'Traits-Composition'
}

{ #category : #'instance creation' }
TraitComposition class >> with: aTraitTransformation [
	^self new
		add: aTraitTransformation;
		yourself
]

{ #category : #'instance creation' }
TraitComposition class >> with: aTraitTransformation with: anotherTraitTransformation [
	^self new
		add: aTraitTransformation;
		add: anotherTraitTransformation;
		yourself
]

{ #category : #composition }
TraitComposition >> + aTraitExpression [
	^ aTraitExpression addCompositionOnLeft: self.

]

{ #category : #composition }
TraitComposition >> - anArray [
	"the modifier operators #@ and #- bind stronger than +"
	
	self transformations: (self transformations collect: [ :t | t - anArray ])
]

{ #category : #composition }
TraitComposition >> @ anArrayOfAssociations [
	"the modifier operators #@ and #- bind stronger than +"

	self transformations: (self transformations collect: [ :t | t @ anArrayOfAssociations ]).
]

{ #category : #accessing }
TraitComposition >> add: aTraitTransformation [
	self errorIfNotAddable: aTraitTransformation.
	self transformations addLast: aTraitTransformation
]

{ #category : #testing }
TraitComposition >> addCompositionOnLeft: aTraitComposition [
	self transformations do: [ :each | aTraitComposition add: each ].
	^ aTraitComposition
]

{ #category : #composition }
TraitComposition >> addOnTheLeft: aTrait [ 
	self errorIfNotAddable: aTrait.
	self transformations addFirst: aTrait
]

{ #category : #enquiries }
TraitComposition >> allSelectors [
	^ Array streamContents: [ :stream | 
		self transformations do: [ :transformation | stream nextPutAll: transformation allSelectors ]]
]

{ #category : #accessing }
TraitComposition >> allTraits [
	^self traits flatCollect: [:trait |
		trait hasTraitComposition
			ifTrue: [trait traitComposition allTraits copyWith: trait]
			ifFalse: [Array with: trait]]
]

{ #category : #converting }
TraitComposition >> asTraitComposition [
	^self
]

{ #category : #'error-handling' }
TraitComposition >> assertValidUser: aBehavior [
	"Assert that this trait composition set for aBehavior
	does not introduce a cycle."
	
	(self allTraits includes: aBehavior) ifTrue: [
		TraitCompositionException signal: 'Cycle in compositions:  The composition (in)directly includes this trait!']
]

{ #category : #enquiries }
TraitComposition >> changedSelectorsComparedTo: oldComposition [
	| changedSelectors traits |
	changedSelectors := IdentitySet new.
	traits := self traits asIdentitySet addAll: oldComposition traits asIdentitySet; yourself.
	traits do: [:each | | oldTransformation newTransformation |
		newTransformation := self transformationOfTrait: each.
		oldTransformation := oldComposition transformationOfTrait: each.
		(newTransformation isNil or: [oldTransformation isNil])
			ifTrue: [
				changedSelectors addAll: each selectors]
			ifFalse: [
				changedSelectors addAll: 
					(newTransformation changedSelectorsComparedTo: oldTransformation)]].
	^changedSelectors
]

{ #category : #copying }
TraitComposition >> copy [
	self error: 'should not be called'.
	^super copy
]

{ #category : #copying }
TraitComposition >> copyTraitExpression [
	| newCopy |
	newCopy := self shallowCopy.
	newCopy transformations: (self transformations collect: [ :each | each copyTraitExpression ]).
	^ newCopy

]

{ #category : #copying }
TraitComposition >> copyWithExclusionOf: aSymbol to: aTrait [
	| composition transformation |
	composition := self copyTraitExpression.
	transformation := (composition transformationOfTrait: aTrait).
	^composition
		remove: transformation;
		add: (transformation addExclusionOf: aSymbol);
		yourself
]

{ #category : #copying }
TraitComposition >> copyWithoutAlias: aSymbol of: aTrait [
	| composition transformation |
	composition := self copyTraitExpression.
	transformation := (composition transformationOfTrait: aTrait).
	^composition
		remove: transformation;
		add: (transformation removeAlias: aSymbol);
		normalizeTransformations;
		yourself
]

{ #category : #'error-handling' }
TraitComposition >> errorIfNotAddable: aTraitTransformation [
	(self includesTrait: aTraitTransformation trait) ifTrue: [
		^TraitCompositionException
			signal: 'Trait ' , aTraitTransformation trait asString, ' already in composition']
]

{ #category : #private }
TraitComposition >> includesMethod: aSelector [
	^ (self methodDescriptionForSelector: aSelector) isNotEmpty
]

{ #category : #private }
TraitComposition >> includesTrait: aTrait [
	^self traits includes: aTrait
]

{ #category : #initialization }
TraitComposition >> initialize [
	super initialize.
	transformations := OrderedCollection new
]

{ #category : #private }
TraitComposition >> isAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in this or in another composition somewhere deeper in 
	the tree of traits compositions."

	| methodDescription |
	methodDescription := (self methodDescriptionsForSelector: aSymbol)
		detect: [:each | each selector = aSymbol].
	^methodDescription isAliasSelector
]

{ #category : #private }
TraitComposition >> isEmpty [
	^self transformations isEmpty
]

{ #category : #private }
TraitComposition >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias defined
	in this composition."

	| methodDescription |
	methodDescription := (self methodDescriptionsForSelector: aSymbol)
		detect: [:each | each selector = aSymbol].
	^methodDescription isLocalAliasSelector
]

{ #category : #enquiries }
TraitComposition >> methodDescriptionForSelector: aSymbol [
	"Return a TraitMethodDescription for the selector aSymbol."

	| description |
	description := TraitMethodDescription selector: aSymbol.
	self transformations do: [:each |
		each collectMethodsFor: aSymbol into: description].
	^description
]

{ #category : #enquiries }
TraitComposition >> methodDescriptionsForSelector: aSymbol [
	"Return a collection of TraitMethodDescriptions for aSymbol and all the 
	aliases of aSymbol."

	| selectors collection |
	selectors := IdentitySet with: aSymbol.
	self transformations do: [:each |
		selectors addAll: (each aliasesForSelector: aSymbol)].
	collection := OrderedCollection new: selectors size.
	selectors do: [:each |
		collection add: (self methodDescriptionForSelector: each)].
	^collection
]

{ #category : #composition }
TraitComposition >> normalizeTransformations [
	self transformations: (
		self transformations collect: [:each |
			each normalized])
]

{ #category : #private }
TraitComposition >> notEmpty [
	^self isEmpty not
]

{ #category : #alias }
TraitComposition >> originSelectorOf: aSelector [
 
	self isEmpty ifTrue: [ ^ aSelector ].
	self transformations do:[ :transformation | 
		transformation allAliasesDict 
			at: aSelector ifPresent: [:value | ^ value ]].
		
	^aSelector
]

{ #category : #copying }
TraitComposition >> postCopy [
	super postCopy.
	transformations := transformations collect: [:each | each copy].
]

{ #category : #printing }
TraitComposition >> printOn: aStream [
	
	self transformations isEmptyOrNil
		ifFalse: [
			
			"The first element does not need parenthesis"
			aStream nextPutAll: (self transformations at: 1) asString.
			(self transformations size > 1) ifTrue: [ aStream nextPutAll: ' + '].
			
			"The rest of the elements either need or not parenthesis depending if they are trait compositions or traits"
			(self transformations allButFirst)
				do: [:each | aStream nextPutAll: (each isTrait 
														ifTrue: [ each asString]
														ifFalse: [ '(', each asString, ')' ])]										 
				separatedBy: [aStream nextPutAll: ' + '] ]
			
		ifTrue: [aStream nextPutAll: '{}']
]

{ #category : #printing }
TraitComposition >> printString [
	^String streamContents: [:stream |
		self printOn: stream]
]

{ #category : #accessing }
TraitComposition >> remove: aTransformation [
	self transformations
		remove: aTransformation
]

{ #category : #accessing }
TraitComposition >> removeFromComposition: aTrait [
	self remove:
		(self transformationOfTrait: aTrait)
]

{ #category : #accessing }
TraitComposition >> size [
	^transformations size
]

{ #category : #comparing }
TraitComposition >> syntacticallyEquals: aTraitComposition [
	"Test syntactic equivalence of this trait composition with another. 
	The order of the transformations is taken into account. For example, A + B ~= B + A."

	self == aTraitComposition ifTrue: [ ^ true ].
	self species = aTraitComposition species ifFalse: [ ^ false ].
	self transformations size = aTraitComposition transformations size ifFalse: [ ^ false ].
	
	self transformations with: aTraitComposition transformations do: [ :mine :their | 
		(mine syntacticallyEquals: their) ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #enquiries }
TraitComposition >> traitProvidingSelector: aSymbol [
	"Return the trait which originally provides the method aSymbol or return nil
	if trait composition does not provide this selector or there is a conflict.
	Take aliases into account. Return the trait which the aliased method is defined in."

	| methodDescription locatedMethod |
	methodDescription := self methodDescriptionForSelector: aSymbol.
	(methodDescription isProvided not or: [methodDescription isConflict])	
		ifTrue: [^nil].
	locatedMethod := methodDescription providedLocatedMethod.
	^locatedMethod methodClass traitOrClassOfSelector: locatedMethod selector
]

{ #category : #accessing }
TraitComposition >> traits [
	^self transformations collect: [:each |
		each trait]
]

{ #category : #accessing }
TraitComposition >> transformationOfTrait: aTrait [
	"Return the transformation which holds aTrait
	or nil if this composition doesn't include aTrait."
	
	^self transformations
		detect: [:each | each trait = aTrait]
		ifNone: [nil]
]

{ #category : #accessing }
TraitComposition >> transformations [
	^transformations
]

{ #category : #testing }
TraitComposition >> transformations: aCollection [
	transformations := aCollection
]
