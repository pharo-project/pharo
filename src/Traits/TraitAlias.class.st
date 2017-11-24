"
See comment of my superclass TraitTransformation.
"
Class {
	#name : #TraitAlias,
	#superclass : #TraitTransformation,
	#instVars : [
		'aliases'
	],
	#category : #'Traits-Composition'
}

{ #category : #'instance creation' }
TraitAlias class >> assertValidAliasDefinition: anArrayOfAssociations [
	"Throw an exceptions if the alias definition is not valid.
	It is expected to be a collection of associations and
	the number of arguments of the alias selector has to
	be the same as the original selector."

	((anArrayOfAssociations isKindOf: Collection) and: [
		anArrayOfAssociations allSatisfy: [:each |
			each isKindOf: Association]]) ifFalse: [
		TraitCompositionException signal: 'Invalid alias definition: Not a collection of associations.'].
	
	(anArrayOfAssociations allSatisfy: [:association |
		(association key numArgs = association value numArgs and: [
			(association key numArgs = -1) not])]) ifFalse: [
		TraitCompositionException signal: 'Invalid alias definition: Alias and original selector have to have the same number of arguments.']
]

{ #category : #'instance creation' }
TraitAlias class >> with: aTraitComposition aliases: anArrayOfAssociations [
	self assertValidAliasDefinition: anArrayOfAssociations.
	^self new
		subject: aTraitComposition;
		aliases: anArrayOfAssociations;
		yourself
]

{ #category : #composition }
TraitAlias >> - anArrayOfSelectors [
	^TraitExclusion
		with: self
		exclusions: anArrayOfSelectors
]

{ #category : #composition }
TraitAlias >> @ anArrayOfAssociations [ 
	^TraitAlias 
		with: subject
		aliases: (anArrayOfAssociations, self aliases)
]

{ #category : #enumeration }
TraitAlias >> aliasNamed: aSymbol ifAbsent: aBlock [
	^self aliases
		detect: [:association |  association key = aSymbol]
		ifNone: aBlock
]

{ #category : #accessing }
TraitAlias >> aliases [
	"Collection of associations where key is the
	alias and value the original selector."
	
	^aliases
]

{ #category : #accessing }
TraitAlias >> aliases: anArrayOfAssociations [
	| newNames |
	newNames := (anArrayOfAssociations collect: [:each | each key]) asIdentitySet.
	newNames size < anArrayOfAssociations size ifTrue: [
		TraitCompositionException signal: 'Cannot use the same alias name twice'].
	anArrayOfAssociations do: [:each |
		(newNames includes: each value) ifTrue: [
			TraitCompositionException signal: 'Cannot define an alias for an alias']].
	aliases := anArrayOfAssociations
]

{ #category : #enquiries }
TraitAlias >> aliasesForSelector: aSymbol [
	| selectors |
	selectors := self aliases
		select: [:association | association value = aSymbol]
		thenCollect: [:association | association key].
	^(super aliasesForSelector: aSymbol)
		addAll: selectors;
		yourself
		 
]

{ #category : #enquiries }
TraitAlias >> allAliasesDict [
	| dict |
	dict := super allAliasesDict.
	self aliases do: [:assoc |
		dict at: assoc key put: assoc value].
	^dict
]

{ #category : #enquiries }
TraitAlias >> allSelectors [
	^self subject allSelectors
		addAll: (self aliases collect: [:each | each key]) asSet;
		yourself
]

{ #category : #enquiries }
TraitAlias >> collectMethodsFor: aSelector into: methodDescription [
	| originalSelector association |
	self subject
		collectMethodsFor: aSelector
		into: methodDescription.			
	association := self aliasNamed: aSelector ifAbsent: [nil].
	association notNil ifTrue: [
		originalSelector := association value.
		self subject
			collectMethodsFor: originalSelector
			into: methodDescription]
]

{ #category : #copying }
TraitAlias >> copyTraitExpression [
	^super copyTraitExpression
		aliases: self aliases deepCopy;
		yourself
]

{ #category : #testing }
TraitAlias >> isEmpty [
	^self aliases isEmpty
]

{ #category : #copying }
TraitAlias >> postCopy [
	super postCopy.
	aliases := self aliases copy
]

{ #category : #printing }
TraitAlias >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPut: $@;
		space;
		nextPut: ${.
	self aliases do: [:each | aStream print: each]
		separatedBy: [aStream nextPutAll: '. '].
	aStream nextPut: $}
]

{ #category : #composition }
TraitAlias >> removeAlias: aSymbol [
	self aliases: (self aliases
		reject: [:each | each key = aSymbol])
]

{ #category : #comparing }
TraitAlias >> syntacticallyEquals: aTraitTransformation [
	"Test syntactic equivalence of this trait transformation with another. 
	The order of aliases is taken into account. For example, A @ { #x -> #y. #z -> #w } ~= A @ { #z -> #w. #x -> #y }."

	self == aTraitTransformation ifTrue: [ ^ true ].
	self species = aTraitTransformation species ifFalse: [ ^ false ].
	^ self aliases = aTraitTransformation aliases
]
