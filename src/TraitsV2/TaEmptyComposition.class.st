"
I am an empty composition
I have no methods or slots :(
"
Class {
	#name : #TaEmptyComposition,
	#superclass : #TaAbstractComposition,
	#category : #'TraitsV2-Compositions'
}

{ #category : #operations }
TaEmptyComposition >> , anotherTrait [
	^ anotherTrait
]

{ #category : #operations }
TaEmptyComposition >> - anArrayOfSelectors [
	self error:'Could not remove a selector from an empty composition'.
]

{ #category : #comparing }
TaEmptyComposition >> = anotherTrait [
	^ self class = anotherTrait class
]

{ #category : #operations }
TaEmptyComposition >> >> anAssociation [
	self error:'Could not perform on empty composition'
]

{ #category : #operations }
TaEmptyComposition >> @ anAssociation [
	self error:'Could not perform on empty composition'
]

{ #category : #operations }
TaEmptyComposition >> @= anAssociation [
	self error:'Could not perform on empty composition'
]

{ #category : #operations }
TaEmptyComposition >> addToSequence: sequence [
	"I am empty I will not add my self to anybody"
	^ sequence
]

{ #category : #users }
TaEmptyComposition >> addUser: aClass [ 

	"Nothing to do.... I am empty"
]

{ #category : #querying }
TaEmptyComposition >> allTraits [
	^ #()
]

{ #category : #testing }
TaEmptyComposition >> changesSourceCode: aString [ 
	
	^ false
]

{ #category : #accessing }
TaEmptyComposition >> classComposition [
	^ self class new.
]

{ #category : #accessing }
TaEmptyComposition >> compiledMethodAt: aSelector [
	NotFound signalFor: aSelector
]

{ #category : #copying }
TaEmptyComposition >> copyTraitExpression [
	^ self
	
]

{ #category : #copying }
TaEmptyComposition >> copyWithoutTrait: aTrait [
	^ self
	
]

{ #category : #comparing }
TaEmptyComposition >> hash [
	^ 1
]

{ #category : #testing }
TaEmptyComposition >> isAliasSelector: aString [ 
	
	^ false
]

{ #category : #testing }
TaEmptyComposition >> isEmpty [
	^ true
]

{ #category : #accessing }
TaEmptyComposition >> methods [
	^ #()
]

{ #category : #users }
TaEmptyComposition >> removeUser: aClass [ 

	"Nothing to do.... I am empty"
]

{ #category : #querying }
TaEmptyComposition >> reverseAlias: aSelector [
	
	^ #()
]

{ #category : #accessing }
TaEmptyComposition >> selectors [
	^#()
]

{ #category : #accessing }
TaEmptyComposition >> slots [
	^ #()
]

{ #category : #printing }
TaEmptyComposition >> traitCompositionExpression [
	
	^ '{}'
]

{ #category : #printing }
TaEmptyComposition >> traitCompositionExpressionWithParens [
	^ self traitCompositionExpression
]

{ #category : #querying }
TaEmptyComposition >> traitDefining: aSelector [
	NotFound signalFor: aSelector
]

{ #category : #querying }
TaEmptyComposition >> traits [
	^ #()
]
