Class {
	#name : #TaCompositionElement,
	#superclass : #TaAbstractComposition,
	#instVars : [
		'innerClass'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'as yet unclassified' }
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

{ #category : #operations }
TaCompositionElement >> >> anAssociation [
	^ TaRenameSlot from:anAssociation key to: anAssociation value on: self.
]

{ #category : #users }
TaCompositionElement >> addUser: aClass [ 
	
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

{ #category : #'transforming selectors' }
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
TaCompositionElement >> copySlot: aSlot [
	| newOne |
	newOne := aSlot copy.
	newOne isVirtual ifFalse: [ newOne index: nil ].
	^ newOne
]

{ #category : #copying }
TaCompositionElement >> copyTraitExpression [
	^ self shallowCopy.
]

{ #category : #copying }
TaCompositionElement >> copyWithoutTrait: aTrait [ 
	
	^ self = aTrait ifTrue: [ TaEmptyComposition new ] ifFalse: [ self copy ]
]

{ #category : #'transforming selectors' }
TaCompositionElement >> dealiasSelector: oldSelector [
	^ oldSelector = self initializeSelectorForMe
		ifTrue: [ ^ #initializeTalent ]
		ifFalse: [ ^ oldSelector ]
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

{ #category : #querying }
TaCompositionElement >> originSelectorOf: aSelector [ 
	
	^ aSelector
]

{ #category : #users }
TaCompositionElement >> removeUser: aClass [ 
	
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
	^ innerClass slots collect: [ :e | self copySlot: e ]
]

{ #category : #printing }
TaCompositionElement >> traitCompositionExpression [
	
	^ innerClass name
]

{ #category : #printing }
TaCompositionElement >> traitCompositionExpressionWithParens [

	^ self traitCompositionExpression
]

{ #category : #accessing }
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
TaCompositionElement >> without: anotherTalent [
	^ self = anotherTalent ifTrue:[nil] ifFalse:[self].
]
