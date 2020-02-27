"
I aliases a method in a trait.
As a result, the new trait includes both methods, the original and the newone with the new selector.
I implement the #@ operator.
"
Class {
	#name : #TaAliasMethod,
	#superclass : #TaSingleComposition,
	#instVars : [
		'aliases'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaAliasMethod class >> alias:anArrayOfAssociations to: aTrait [
	^ self new
		aliases: anArrayOfAssociations asDictionary;
		inner: aTrait;
		checkAssociations;
		yourself.
]

{ #category : #'transforming selectors' }
TaAliasMethod >> aliasSelector: aSelector [
	^ (self hasAliasForOld: aSelector)
		ifTrue: [ self newSelectorFor: aSelector ]
		ifFalse: [ inner aliasSelector: aSelector ]
]

{ #category : #accessing }
TaAliasMethod >> aliases [
	^ aliases
]

{ #category : #accessing }
TaAliasMethod >> aliases: anObject [
	aliases := anObject
]

{ #category : #'transforming selectors' }
TaAliasMethod >> changesSourceCode: aSelector [ 
	
	^ self hasAliasForNew: aSelector

]

{ #category : #validation }
TaAliasMethod >> checkAssociations [
	"I check the validity of the associations used for aliasing"
	self aliases
		associationsDo: [ :assoc | 
			| old new |
			new := assoc key.
			old := assoc value.
			new = old ifTrue: [ self error: 'The alias and the alieased are the same' ].
			(self aliases includesKey: old) ifTrue: [ self error: 'An alias is refering other alias' ].
			new numArgs = old numArgs ifFalse: [ self error: 'Number of parameters in aliases should be the same.' ].
			"For now I am not checking that the aliased method exists, 
			this is the same behavior than before.
			(inner compiledMethodAt: old) ifNil: [ NotFound signalFor: old ]" ]
]

{ #category : #accessing }
TaAliasMethod >> compiledMethodAt: aSelector [

	^ (self hasAliasForNew: aSelector)
		ifTrue: [ inner compiledMethodAt: (self oldSelectorFor: aSelector) ]
		ifFalse: [ inner compiledMethodAt: aSelector ]
]

{ #category : #copying }
TaAliasMethod >> copyTraitExpression [
	^ self class alias: aliases to: inner

]

{ #category : #copying }
TaAliasMethod >> copyWithoutTrait: aTrait [
 
	^ self inner = aTrait ifTrue: [ TaEmptyComposition new ] ifFalse: [ self copy ]
]

{ #category : #accessing }
TaAliasMethod >> existingAliasedSelectors [
	| innerSelectors |
	"I calculate the new aliases selectors"
	innerSelectors := inner selectors.

	^ self aliases associations
		select: [ :a | innerSelectors includes: a value ]
		thenCollect: [ :a | a key ]
]

{ #category : #testing }
TaAliasMethod >> hasAliasForNew: aSelector [
	"Check if there is a alias for a new selector"
	^ aliases includesKey: aSelector
]

{ #category : #testing }
TaAliasMethod >> hasAliasForOld: aSelector [
	"Check if there is a alias for a old selector"
	^ aliases values includes: aSelector
]

{ #category : #testing }
TaAliasMethod >> isAliasSelector: aSelector [

	^ (self aliases includesKey: aSelector) or: [ inner isAliasSelector: aSelector ]
]

{ #category : #testing }
TaAliasMethod >> isLocalAliasSelector: aSymbol [
	^ (self hasAliasForNew: aSymbol) or: [ inner isLocalAliasSelector: aSymbol ]
]

{ #category : #'instance creation' }
TaAliasMethod >> newSelectorFor: aSelector [
	^ aliases keyAtValue: aSelector
]

{ #category : #aliasing }
TaAliasMethod >> oldSelectorFor: aSelector [
	^ aliases at: aSelector
]

{ #category : #querying }
TaAliasMethod >> originSelectorOf: aSelector [

	^ aliases at: aSelector ifAbsent: [ aSelector ]
	
	
]

{ #category : #querying }
TaAliasMethod >> reverseAlias: aSelector [
	^ (inner reverseAlias: aSelector) , (aliases associations select: [ :a | a value = aSelector ] thenCollect: #key)
]

{ #category : #accessing }
TaAliasMethod >> selectors [
	^ inner selectors , self existingAliasedSelectors. 
]

{ #category : #accessing }
TaAliasMethod >> sourceCodeAt: aSelector [
	^ (self hasAliasForNew: aSelector)
		ifTrue: [ (inner compiledMethodAt: (self oldSelectorFor: aSelector))
				getSourceReplacingSelectorWith: aSelector ]
		ifFalse: [ super sourceCodeAt: aSelector ]
]

{ #category : #printing }
TaAliasMethod >> traitCompositionExpression [
	
	^ self inner traitCompositionExpressionWithParens , ' @ ' , aliases associations printString
]

{ #category : #querying }
TaAliasMethod >> traitDefining: aSelector [
	^ (self hasAliasForNew: aSelector)
		ifTrue: [ inner traitDefining: (self oldSelectorFor: aSelector) ]
		ifFalse: [ inner traitDefining: aSelector ]
]
