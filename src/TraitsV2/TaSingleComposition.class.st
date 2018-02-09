"
I am the superclass of all the operations that modifies a single trait.
"
Class {
	#name : #TaSingleComposition,
	#superclass : #TaAbstractComposition,
	#instVars : [
		'inner'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #comparing }
TaSingleComposition >> = another [
	^ (another class = self class) and: [ another inner = inner ]
]

{ #category : #users }
TaSingleComposition >> addUser: aClass [ 

	inner addUser: aClass
]

{ #category : #'transforming selectors' }
TaSingleComposition >> aliasSelector: aSelector [
	^ inner aliasSelector: aSelector.
]

{ #category : #querying }
TaSingleComposition >> allTraits [
	^ inner allTraits
]

{ #category : #testing }
TaSingleComposition >> changesSourceCode: aSelector [ 
	
	^ inner changesSourceCode: aSelector
]

{ #category : #accessing }
TaSingleComposition >> classComposition [
	^ inner classComposition
]

{ #category : #accessing }
TaSingleComposition >> compiledMethodAt: aSelector [
	^ inner compiledMethodAt: aSelector.
]

{ #category : #copying }
TaSingleComposition >> copyWithoutTrait: aTrait [
	(self = aTrait or: [ inner = aTrait ])
		ifTrue: [ ^ inner copyWithoutTrait: aTrait ].
		
	^ self copy
]

{ #category : #comparing }
TaSingleComposition >> hash [
	^ inner hash
]

{ #category : #accessing }
TaSingleComposition >> initializeSelectorForMe [
	^ inner initializeSelectorForMe.
]

{ #category : #accessing }
TaSingleComposition >> inner [
	^ inner
]

{ #category : #accessing }
TaSingleComposition >> inner: anObject [
	inner := anObject
]

{ #category : #testing }
TaSingleComposition >> isAliasSelector: aSymbol [
	^ inner isAliasSelector: aSymbol 
]

{ #category : #testing }
TaSingleComposition >> isLocalAliasSelector: aSymbol [
	^ inner isLocalAliasSelector: aSymbol 
]

{ #category : #accessing }
TaSingleComposition >> methods [
	^ inner methods
]

{ #category : #accessing }
TaSingleComposition >> name [ 
	^ inner name
]

{ #category : #querying }
TaSingleComposition >> originSelectorOf: aSelector [

	^ inner originSelectorOf: aSelector

]

{ #category : #users }
TaSingleComposition >> removeUser: aClass [ 

	inner removeUser: aClass
]

{ #category : #querying }
TaSingleComposition >> reverseAlias: aSelector [
	
	^ inner reverseAlias: aSelector
]

{ #category : #accessing }
TaSingleComposition >> selectors [
	^ inner selectors
]

{ #category : #accessing }
TaSingleComposition >> slots [
	^ inner slots. 
]

{ #category : #accessing }
TaSingleComposition >> traitDefining: selector [
	^ inner traitDefining: selector
]

{ #category : #querying }
TaSingleComposition >> traits [
	^ inner traits
]

{ #category : #accessing }
TaSingleComposition >> without: anotherTalent [
	^ (anotherTalent = self or: [ inner = anotherTalent ])
		ifTrue: [ nil ]
		ifFalse: [ self ]
]
