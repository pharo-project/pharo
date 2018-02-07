"
Used by TraitComposition to encapsulates a collection of methods for one particular selector when querying for changes. According to the number and kind of those methods a provided method exists, there is a conflict or there are no provided nor conflicting methods at all. I provide the interface to query for those situations, e.g., effectiveMethod returns the provided method or the conflict marker method.

"
Class {
	#name : #TraitMethodDescription,
	#superclass : #Object,
	#instVars : [
		'selector',
		'locatedMethods'
	],
	#category : #'Traits-Composition'
}

{ #category : #private }
TraitMethodDescription class >> maxArguments [
	^30
]

{ #category : #'instance creation' }
TraitMethodDescription class >> new [
	^super new
		initialize;
		yourself
]

{ #category : #'instance creation' }
TraitMethodDescription class >> selector: aSymbol [
	^self new
		selector: aSymbol
		yourself
]

{ #category : #accessing }
TraitMethodDescription >> addLocatedMethod: aLocatedMethod [
	locatedMethods add: aLocatedMethod
]

{ #category : #accessing }
TraitMethodDescription >> conflictMethod [
	| binary numberOfArguments |
	self isConflict ifFalse: [^nil].
	binary := self isBinarySelector.
	numberOfArguments := self numberOfArguments.
	
	^self
		generateMethod: self selector
		withMarker: CompiledMethod conflictMarker
		forArgs: numberOfArguments
		binary: binary
]

{ #category : #accessing }
TraitMethodDescription >> effectiveMethod [
	"Return the effective compiled method of this method description." 

	| method |
	method := self providedMethod.
	method isNil ifFalse: [^ method].
	method := self conflictMethod.
	method isNil ifFalse: [^ method].
	^ self requiredMethod.
]

{ #category : #accessing }
TraitMethodDescription >> effectiveMethodCategory [
	^ self effectiveMethodCategoryCurrent: nil new: nil
]

{ #category : #accessing }
TraitMethodDescription >> effectiveMethodCategoryCurrent: currentCategoryOrNil new: newCategoryOrNil [
	| result size isCurrent isConflict |
	size := self size.
	size = 0 ifTrue: [^ nil].
	result := self locatedMethods anyOne category.
	size = 1 ifTrue: [^ result].
	
	isCurrent := currentCategoryOrNil isNil.
	isConflict := false.
	self locatedMethods do: [:each | | cat |
		cat := each category.
		isCurrent := isCurrent or: [cat == currentCategoryOrNil].
		isConflict := isConflict or: [cat ~~ result]].
	isConflict ifFalse: [^ result].
	(isCurrent not and: [newCategoryOrNil notNil]) ifTrue: [^ newCategoryOrNil].
	
	^ Protocol ambiguous 
]

{ #category : #private }
TraitMethodDescription >> generateMethod: aSelector withMarker: aSymbol forArgs: aNumber binary: aBoolean [
	| source node keywords |
	source := String streamContents: [:stream |
		aNumber < 1
			ifTrue: [stream nextPutAll: aSelector]
			ifFalse: [aBoolean
				ifTrue: [
					stream nextPutAll: aSelector; space; nextPutAll: 'anObject']
				ifFalse: [
					keywords := aSelector keywords.
					1 to: aNumber do: [:argumentNumber |
						stream
							nextPutAll: (keywords at: argumentNumber); space;
							nextPutAll: 'arg'; nextPutAll: argumentNumber asString; space]]].
		stream cr; tab; nextPutAll: 'self '; nextPutAll: aSymbol].
	
	node := self class compiler
		source: source;
		class: self class;
		failBlock: [];
		translate.
	^(node generateWithSource) selector: aSelector; yourself
]

{ #category : #initialization }
TraitMethodDescription >> initialize [
	super initialize.
	locatedMethods := IdentitySet new
]

{ #category : #testing }
TraitMethodDescription >> isAliasSelector [
	"Return true if the selector is an alias (if it is different
	from the original selector) or already an aliased method
	in the original location (recursively search the compositions).
	Return false, if not or if we have a conflict."
	
	| locatedMethod |
	^self size = 1 and: [
		locatedMethod := self locatedMethods anyOne.
		(locatedMethod selector ~= self selector) or: [
			locatedMethod methodClass isAliasSelector: self selector]]
]

{ #category : #testing }
TraitMethodDescription >> isBinarySelector [
	^self locatedMethods anyOne
		isBinarySelector
]

{ #category : #testing }
TraitMethodDescription >> isConflict [
	| count originMethodReferenciel |
	count := 0.

	self locatedMethods size > 1
		ifTrue: 
			["If they are more than 1 located method, then check whether these methods have the same origin"
			originMethodReferenciel := self locatedMethods anyOne  origin.
			(self locatedMethods allSatisfy: [:each | each origin == originMethodReferenciel])
				ifTrue: [ ^ false ]].

	self methodsDo: [:each |
		each isProvided ifTrue: [
			count := count + 1.
			count > 1 ifTrue: [^true]]].
	^false
]

{ #category : #testing }
TraitMethodDescription >> isEmpty [
	^self size = 0
]

{ #category : #testing }
TraitMethodDescription >> isLocalAliasSelector [
	"Return true if the selector is an alias (if it is different
	from the original selector). Return false, if not or if we
	have a conflict."
	
	^self size = 1 and: [
		(self locatedMethods anyOne selector ~= self selector)]
]

{ #category : #testing }
TraitMethodDescription >> isNotEmpty [
	^self size > 0
]

{ #category : #testing }
TraitMethodDescription >> isProvided [
	^ self providedMethod notNil
]

{ #category : #testing }
TraitMethodDescription >> isRequired [
	self isEmpty ifTrue: [^ false].
	^ self locatedMethods allSatisfy: [:each | each isRequired]
]

{ #category : #accessing }
TraitMethodDescription >> locatedMethods [
	^locatedMethods
]

{ #category : #enumeration }
TraitMethodDescription >> methodsDo: aBlock [
	self locatedMethods do: [:each |
		aBlock value: each ]
]

{ #category : #private }
TraitMethodDescription >> numberOfArguments [
	^ self locatedMethods anyOne selector numArgs
		
]

{ #category : #accessing }
TraitMethodDescription >> providedLocatedMethod [
	| locatedMethod aLocatedMethod refOrigin |
	locatedMethod := nil.
	self locatedMethods ifEmpty: [ ^ nil].

	self locatedMethods size > 1
		ifTrue: [ 	aLocatedMethod := self locatedMethods anyOne.
						refOrigin := (aLocatedMethod methodClass >> aLocatedMethod selector) origin.
						(self locatedMethods 
							allSatisfy: [:each | each origin == refOrigin])
						ifTrue: [^ aLocatedMethod].  ].
	
	self locatedMethods do: [:each |
		each isProvided ifTrue: [
			locatedMethod isNil ifFalse: [^nil].
			locatedMethod := each]].
	^locatedMethod
]

{ #category : #accessing }
TraitMethodDescription >> providedMethod [
	^self providedLocatedMethod ifNotNil: [:locatedMethod | locatedMethod method]
]

{ #category : #accessing }
TraitMethodDescription >> requiredMethod [
	| numberOfArguments binary |
	self isRequired ifFalse: [^nil].
	self size = 1 ifTrue: [^self locatedMethods anyOne].

	binary := self isBinarySelector.
	numberOfArguments := self numberOfArguments.
	
	 ^self
		generateMethod: self selector
		withMarker: CompiledMethod implicitRequirementMarker
		forArgs: numberOfArguments
		binary: binary.
]

{ #category : #accessing }
TraitMethodDescription >> selector [
	^selector
]

{ #category : #accessing }
TraitMethodDescription >> selector: aSymbol [ 
	selector := aSymbol
]

{ #category : #accessing }
TraitMethodDescription >> size [
	^self locatedMethods size
]
