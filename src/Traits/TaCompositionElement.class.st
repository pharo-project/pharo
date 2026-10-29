"
I represent the root element in a TraitComposition.
I wrap a class or Trait to be used as a trait in a trait composition.
"
Class {
	#name : 'TaCompositionElement',
	#superclass : 'TaAbstractComposition',
	#instVars : [
		'innerClass'
	],
	#category : 'Traits-Compositions',
	#package : 'Traits',
	#tag : 'Compositions'
}

{ #category : 'instance creation' }
TaCompositionElement class >> for: aClass [
	^ self new
		innerClass: aClass;
		yourself
]

{ #category : 'comparing' }
TaCompositionElement >> = aTrait [
	^ (aTrait species = self species)
		ifTrue: [ aTrait innerClass = self innerClass ]
		ifFalse: [ ^ false ]
]

{ #category : 'users' }
TaCompositionElement >> addUser: aClass [
	"When a user is added to me I propagate to the real Trait, as I am the root of the TraitComposition"
	innerClass addUser: aClass
]

{ #category : 'transforming selectors' }
TaCompositionElement >> aliasSelector: selector [
	^ selector = #initializeTalent
		ifTrue: [ self initializeSelectorForMe ]
		ifFalse: [ selector ]
]

{ #category : 'querying' }
TaCompositionElement >> allTraits [
	^ { innerClass } , innerClass traitComposition allTraits
]

{ #category : 'testing' }
TaCompositionElement >> changesSourceCode: compilationInfo [

	^ false
]

{ #category : 'accessing' }
TaCompositionElement >> classComposition [

	^ TaClassCompositionElement for: innerClass class
]

{ #category : 'accessing' }
TaCompositionElement >> compiledMethodAt: selector [
	| newSelector |
	newSelector := self dealiasSelector: selector.
	^ innerClass methods detect: [ :e | e selector = newSelector ] ifNone: [ innerClass traitComposition compiledMethodAt: newSelector ]
]

{ #category : 'copying' }
TaCompositionElement >> copyTraitExpression [
	"I can be just shallowCopy, because I have only a reference to the real Trait"
	^ self shallowCopy
]

{ #category : 'copying' }
TaCompositionElement >> copyWithoutTrait: aTrait [

	"If you want to remove a trait that is equivalent to me, I return an empty composition.
	If not a copy is returned"
	^ self = aTrait ifTrue: [ TaEmptyComposition new ] ifFalse: [ self copy ]
]

{ #category : 'transforming selectors' }
TaCompositionElement >> dealiasSelector: oldSelector [
	"Usefule to dealias the initialize selector. Used mostly with stateful Talents"

	(oldSelector beginsWith: 'initializeTalent_') ifFalse: [ ^ oldSelector ].

	^ oldSelector = self initializeSelectorForMe
		  ifTrue: [ #initializeTalent ]
		  ifFalse: [ oldSelector ]
]

{ #category : 'comparing' }
TaCompositionElement >> hash [
	^ self innerClass hash
]

{ #category : 'testing' }
TaCompositionElement >> includesTrait: aTrait [

	innerClass = aTrait ifTrue: [ ^ true ].

	^ innerClass traitComposition includesTrait: aTrait
]

{ #category : 'transforming selectors' }
TaCompositionElement >> initializeSelectorForMe [
	^ ('initializeTalent_' , self name) asSymbol
]

{ #category : 'accessing' }
TaCompositionElement >> innerClass [
	^ innerClass
]

{ #category : 'accessing' }
TaCompositionElement >> innerClass: anObject [
	innerClass := anObject
]

{ #category : 'testing' }
TaCompositionElement >> isAliasSelector: aString [

	^ innerClass traitComposition isAliasSelector: aString
]

{ #category : 'accessing' }
TaCompositionElement >> methods [
	^innerClass methods
]

{ #category : 'accessing' }
TaCompositionElement >> methodsCompilationInfo [

	| innerInfos selector traitInfos localSelectors methods |
	"We first collect the compilation info for the local methods I have."
	innerInfos := innerClass localMethods collect: [ :method |
			              TraitMethodCompilationInfo new
				              selector: method selector;
				              definingTrait: self;
				              compiledMethod: method;
				              sourceCode: method sourceCode;
				              yourself ].

	"If I need to alias the talent initializer, I do it now."
	selector := self initializeSelectorForMe.
	innerInfos do: [ :info |
			info selector = #initializeTalent ifTrue: [
					info
						selector: selector;
						sourceCode: (info compiledMethod getSourceReplacingSelectorWith: selector) ] ].

	"Then I collect the ones of my trait composition "
	localSelectors := innerInfos collect: [ :info | info selector ].
	traitInfos := innerClass traitComposition methodsCompilationInfo reject: [ :info | localSelectors includes: info selector ].

	"In order to copy methods right, I update the compiled methods of my trait composition to set my own compiled methods."
	methods := innerClass methods.
	traitInfos do: [ :info |
			methods
				detect: [ :method | method selector = info selector ]
				ifFound: [ :method | info compiledMethod: method ] ].

	^ innerInfos , traitInfos
]

{ #category : 'accessing' }
TaCompositionElement >> name [
	^ innerClass name
]

{ #category : 'testing' }
TaCompositionElement >> needsRecompilation: compilationInfo [
	"We need to recompile if there is a supersend"

	^ innerClass
		  compiledMethodAt: compilationInfo selector
		  ifPresent: [ :aMethod | aMethod sendsToSuper or: [ super needsRecompilation: compilationInfo ] ]
		  ifAbsent: [ true ]
]

{ #category : 'users' }
TaCompositionElement >> removeUser: aClass [
	"When a user is removed from me I propagate to the real Trait, as I am the root of the TraitComposition"
	innerClass removeUser: aClass
]

{ #category : 'querying' }
TaCompositionElement >> reverseAlias: aSelector [

	^ #()
]

{ #category : 'accessing' }
TaCompositionElement >> selectors [
	"We do not rely on the methods of the innerclass because while removing a trait we might return the removed methods during the removal and I need to work properly during the method dictionary rebuild"

	^ innerClass localSelectors , innerClass traitComposition selectors collect: [ :selector | self aliasSelector: selector ]
]

{ #category : 'accessing' }
TaCompositionElement >> slots [
	^ innerClass allSlots
]

{ #category : 'printing' }
TaCompositionElement >> traitCompositionExpression [

	^ innerClass name
]

{ #category : 'printing' }
TaCompositionElement >> traitCompositionExpressionWithParens [

	^ self traitCompositionExpression
]

{ #category : 'querying' }
TaCompositionElement >> traitDefining: selector [
	| newSelector |
	newSelector := self dealiasSelector: selector.

	^ innerClass localMethods
		detect: [ :e | e selector = newSelector ]
		ifFound: [ :e | self ]
		ifNone: [ innerClass traitComposition traitDefining: newSelector ]
]

{ #category : 'querying' }
TaCompositionElement >> traits [
	^ { innerClass }
]

{ #category : 'deprecated' }
TaCompositionElement >> transformations [
	^ { self innerClass }
]

{ #category : 'operations' }
TaCompositionElement >> without: anotherTrait [
	^ self = anotherTrait ifTrue:[nil] ifFalse:[self]
]
