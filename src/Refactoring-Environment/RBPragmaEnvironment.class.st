"
I am a RBBrowserEnvironment on a set of Pragmas.
I containt all entities that define methods using this pragmas.
Example:
(RBBrowserEnvironment new) forPragmas:{ #primitive:}.
"
Class {
	#name : #RBPragmaEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'keywords',
		'condition'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBPragmaEnvironment class >> keywords: aKeywordCollection [
	^ self
		onEnvironment: self default
		keywords: aKeywordCollection
]

{ #category : #'instance creation' }
RBPragmaEnvironment class >> onEnvironment: anEnvironment keywords: aKeywordCollection [
	^ (self onEnvironment: anEnvironment)
		keywords: aKeywordCollection;
		yourself
]

{ #category : #adding }
RBPragmaEnvironment >> addKeyword: aSymbol [
	keywords add: aSymbol
]

{ #category : #initialization }
RBPragmaEnvironment >> condition: aBlock [
	condition := aBlock
]

{ #category : #private }
RBPragmaEnvironment >> defaultLabel [
	| stream |
	stream := String new writeStream.
	keywords do: [ :each | stream nextPut: $<; nextPutAll: each; nextPut: $>; nextPut: $ ].
	^ stream contents
]

{ #category : #testing }
RBPragmaEnvironment >> definesClass: aClass [
	^ environment definesClass: aClass
]

{ #category : #testing }
RBPragmaEnvironment >> includesCategory: aCategory [
	^ (environment includesCategory: aCategory) and: [ (self classNamesFor: aCategory) notEmpty ]
]

{ #category : #testing }
RBPragmaEnvironment >> includesClass: aClass [
	^ (environment includesClass: aClass) and: [ aClass selectors anySatisfy: [ :each | self includesSelector: each in: aClass ] ]
]

{ #category : #testing }
RBPragmaEnvironment >> includesPragma: aPragma [
	^ (keywords includes: aPragma selector) and: [ condition value: aPragma ]
]

{ #category : #testing }
RBPragmaEnvironment >> includesProtocol: aProtocol in: aClass [
	^ (environment includesProtocol: aProtocol in: aClass) and: [ (self selectorsFor: aProtocol in: aClass) notEmpty ]
]

{ #category : #testing }
RBPragmaEnvironment >> includesSelector: aSelector in: aClass [
	(environment includesSelector: aSelector in: aClass)
		ifFalse: [ ^ false ].
	^ (aClass compiledMethodAt: aSelector) pragmas
		anySatisfy: [ :each | self includesPragma: each ]
]

{ #category : #initialization }
RBPragmaEnvironment >> initialize [
	super initialize.
	keywords := IdentitySet new.
	condition := [ :pragma | true ]
]

{ #category : #initialization }
RBPragmaEnvironment >> keywords: aCollection [
	keywords addAll: aCollection
]

{ #category : #testing }
RBPragmaEnvironment >> packages [
	^ (self methods collect: #package) asSet
]

{ #category : #copying }
RBPragmaEnvironment >> postCopy [
	super postCopy.
	keywords := keywords copy
]

{ #category : #printing }
RBPragmaEnvironment >> storeOn: aStream [
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream nextPutAll: ' keywords: '.
	keywords asArray storeOn: aStream.
	aStream nextPut: $)
]
