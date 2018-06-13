"
I represent the root element in a TraitComposition.
I wrap a class or Trait to be used as a trait in a trait composition.
"
Class {
	#name : #TaCompositionElement,
	#superclass : #TaAbstractComposition,
	#instVars : [
		'innerClass'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaCompositionElement class >> for: aClass [
	^ self new
		innerClass: aClass;
		yourself
]

{ #category : #comparing }
TaCompositionElement >> = aTrait [
	^ (aTrait species = self species)
		ifTrue: [ aTrait innerClass = self innerClass ]
		ifFalse: [ ^ false ]
]

{ #category : #users }
TaCompositionElement >> addUser: aClass [ 
	"When a user is added to me I propagate to the real Trait, as I am the root of the TraitComposition"
	innerClass addUser: aClass
]

{ #category : #'transforming selectors' }
TaCompositionElement >> aliasSelector: selector [
	^ selector = #initializeTalent
		ifTrue: [ self initializeSelectorForMe ]
		ifFalse: [ selector ]
]

{ #category : #querying }
TaCompositionElement >> allTraits [
	^ { innerClass } , innerClass traitComposition allTraits.
]

{ #category : #testing }
TaCompositionElement >> changesSourceCode: aSelector [

	^ false 
	

]

{ #category : #accessing }
TaCompositionElement >> classComposition [

	^ TaClassCompositionElement for: innerClass class
]

{ #category : #accessing }
TaCompositionElement >> compiledMethodAt: selector [
	| newSelector |
	newSelector := self dealiasSelector: selector.
	^ innerClass methods detect: [ :e | e selector = newSelector ] ifNone: [ innerClass traitComposition compiledMethodAt: newSelector ]
]

{ #category : #copying }
TaCompositionElement >> copyTraitExpression [
	"I can be just shallowCopy, because I have only a reference to the real Trait"
	^ self shallowCopy.
]

{ #category : #copying }
TaCompositionElement >> copyWithoutTrait: aTrait [ 
	
	"If you want to remove a trait that is equivalent to me, I return an empty composition.
	If not a copy is returned"
	^ self = aTrait ifTrue: [ TaEmptyComposition new ] ifFalse: [ self copy ]
]

{ #category : #'transforming selectors' }
TaCompositionElement >> dealiasSelector: oldSelector [
	"Usefule to dealias the initialize selector. Used mostly with stateful Talents"

	^ oldSelector = self initializeSelectorForMe
		ifTrue: [ ^ #initializeTalent ]
		ifFalse: [ ^ oldSelector ]
]

{ #category : #comparing }
TaCompositionElement >> hash [
	^ self innerClass hash
]

{ #category : #'transforming selectors' }
TaCompositionElement >> initializeSelectorForMe [
	^ ('initializeTalent_' , self name) asSymbol
]

{ #category : #accessing }
TaCompositionElement >> innerClass [
	^ innerClass
]

{ #category : #accessing }
TaCompositionElement >> innerClass: anObject [
	innerClass := anObject
]

{ #category : #testing }
TaCompositionElement >> isAliasSelector: aString [ 
	
	^ innerClass traitComposition isAliasSelector: aString
]

{ #category : #accessing }
TaCompositionElement >> methods [
	^innerClass methods
]

{ #category : #accessing }
TaCompositionElement >> name [
	^ innerClass name
]

{ #category : #users }
TaCompositionElement >> removeUser: aClass [ 
	"When a user is removed from me I propagate to the real Trait, as I am the root of the TraitComposition"	
	innerClass removeUser: aClass
]

{ #category : #querying }
TaCompositionElement >> reverseAlias: aSelector [
	
	^ #()
]

{ #category : #accessing }
TaCompositionElement >> selectors [
	" I get all the selectors of the methods in this talent, if there is a #initializeTalent selector I rename it to #initializeTalent_NameOfTalent"

	| originals |
	originals := self methods collect: #selector.
	^ (originals includes: #initializeTalent)
		ifTrue: [ (originals reject: [ :e | e = #initializeTalent ]) copyWith: self initializeSelectorForMe ]
		ifFalse: [ originals ]
]

{ #category : #accessing }
TaCompositionElement >> slots [
	^ innerClass slots 
]

{ #category : #printing }
TaCompositionElement >> traitCompositionExpression [
	
	^ innerClass name
]

{ #category : #printing }
TaCompositionElement >> traitCompositionExpressionWithParens [

	^ self traitCompositionExpression
]

{ #category : #querying }
TaCompositionElement >> traitDefining: selector [
	| newSelector |
	newSelector := self dealiasSelector: selector.
	
	^ innerClass localMethods
		detect: [ :e | e selector = newSelector ]
		ifFound: [ :e | self ]
		ifNone: [ innerClass traitComposition traitDefining: newSelector ]
]

{ #category : #querying }
TaCompositionElement >> traits [
	^ { innerClass }
]

{ #category : #operations }
TaCompositionElement >> without: anotherTrait [
	^ self = anotherTrait ifTrue:[nil] ifFalse:[self].
]
