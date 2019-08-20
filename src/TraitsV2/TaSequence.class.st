"
I represent a sequence of traits in a trait composition.
When two traits are operated with + I appear.
The methods and slots are the union of my inner members.
I solve when there are conflicts.
"
Class {
	#name : #TaSequence,
	#superclass : #TaAbstractComposition,
	#instVars : [
		'members'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaSequence class >> with: initialValue [
	^ self new 
		addMember: initialValue;
		yourself.
]

{ #category : #'instance creation' }
TaSequence class >> withAll: initialMembers [

	initialMembers ifEmpty: [ ^ TaEmptyComposition new ].

	^ self new 
		addAll: initialMembers;
		yourself.
]

{ #category : #operations }
TaSequence >> + anotherTrait [
	^ anotherTrait asTraitComposition addToSequence: self copyTraitExpression.
	
]

{ #category : #operations }
TaSequence >> , anotherTrait [
	^ anotherTrait addToSequence: self copy.

]

{ #category : #comparing }
TaSequence >> = anotherTrait [
	^ anotherTrait species = self species and: [ self members = anotherTrait members ]
]

{ #category : #adding }
TaSequence >> addAll: aCollection [
	"Adds all the members in the othe sequence"
	aCollection do:[:e | self addMember: e].
]

{ #category : #adding }
TaSequence >> addMember: aTrait [
	"Adds a single trait to me"
	self validateDuplications: aTrait.
	self validateSlots: aTrait.
	
	members add: aTrait
]

{ #category : #operations }
TaSequence >> addToSequence: sequence [
	"I add my self to another sequence. When adding I add all my members, not myself"
	members do:[:e |sequence addMember: e].
	^ sequence

]

{ #category : #users }
TaSequence >> addUser: aClass [ 
	"I propagate the users to all my members"
	members do: [ :m | m addUser: aClass ]
]

{ #category : #'transforming selectors' }
TaSequence >> aliasSelector: aSelector [
	^ members inject: aSelector into:[:acc :each | each aliasSelector: acc]. 
]

{ #category : #querying }
TaSequence >> allTraits [
	^ members flatCollect: #allTraits
]

{ #category : #accessing }
TaSequence >> categoryOfMethod: method withSelector: selector [
	| categories |
	
	categories := (self methods select: [ :e | e selector = selector ] thenCollect: #category) asSet.

	^ categories size = 1
		ifTrue: [ ^ categories anyOne ]
		ifFalse: [ Protocol ambiguous ]
]

{ #category : #testing }
TaSequence >> changesSourceCode: aSelector [
	"If the selector has conflicts it should be recompiled"
	^ (self isConflictingSelector: aSelector)
		or: [ self members anySatisfy: [ :e | e changesSourceCode: aSelector ] ]
]

{ #category : #accessing }
TaSequence >> classComposition [
	^ self class withAll: (members collect:#classComposition)
]

{ #category : #accessing }
TaSequence >> compiledMethodAt: aSelector [
	
	^ (self memberForSelector: aSelector) compiledMethodAt: aSelector
]

{ #category : #copying }
TaSequence >> copyTraitExpression [
	^ self class withAll: (members collect:#copyTraitExpression)
]

{ #category : #copying }
TaSequence >> copyWithoutTrait: aTrait [
	| newMembers |

	newMembers := members collect: [ :e | e copyWithoutTrait: aTrait ] thenReject: #isEmpty.

	^ self class withAll: newMembers
	

]

{ #category : #comparing }
TaSequence >> hash [
	^ self class hash
]

{ #category : #initialization }
TaSequence >> initialize [
	super initialize.
	members := OrderedCollection new.
]

{ #category : #operations }
TaSequence >> initializeObject: anObject [
	members do: [ :e | e initializeObject: anObject ]
]

{ #category : #testing }
TaSequence >> isAliasSelector: aSelector [
 
	^ self members anySatisfy: [ :e | e isAliasSelector: aSelector ]
]

{ #category : #testing }
TaSequence >> isConflictingSelector: aSelector [
	"I test if a given selector is defined in more than one of my members.
	Two or more members can have the same method. 
	They are a conflict if these members came from different origins or if they are different.
	If they came from the same origin, it is not a conflict. Because solving one or the other is the same result."

	| possibleConflicts |
	possibleConflicts := self methods select: [ :each | each selector = aSelector and: [ each isRequired not ] ].
	possibleConflicts size = 1 ifTrue: [ ^ false ].
	
	^ (possibleConflicts collect: [:each | each originMethod ] as: Set) size > 1.
]

{ #category : #testing }
TaSequence >> isLocalAliasSelector: aSymbol [
	^ members anySatisfy: [ :e | e isLocalAliasSelector: aSymbol ]
]

{ #category : #accessing }
TaSequence >> memberForSelector: aSelector [

	"I return the member that defines a given selector.
	I first look for a valid member, if not, I will return the first"
	^ members
		detect:
			[ :e | (e selectors includes: aSelector) and: [ (e compiledMethodAt: aSelector) isRequired not ] ]
		ifNone: [ members detect: [ :e | e selectors includes: aSelector ] ]
]

{ #category : #operations }
TaSequence >> members [
	^ members
]

{ #category : #accessing }
TaSequence >> methods [
	^ members flatCollect: #methods
]

{ #category : #accessing }
TaSequence >> name [
	^ '_' join: (members collect:#name) 
]

{ #category : #querying }
TaSequence >> originSelectorOf: aSelector [

	^ (self traitDefining: aSelector ifNone: [ ^aSelector ]) originSelectorOf: aSelector

]

{ #category : #users }
TaSequence >> removeUser: aClass [ 
	"I propagate to my members the removal of an user"
	self members do: [ :m | m removeUser: aClass] 
]

{ #category : #querying }
TaSequence >> reverseAlias: aSelector [
	
	^ self members flatCollect: [ :e | e reverseAlias: aSelector ]
]

{ #category : #accessing }
TaSequence >> selectorForConflict: selector [
	| args keywords |
	
	"I calculate the selector to use when there is a conflict. 
	I have to reimplement a similar code that is in RBParser, as the parser cannot be available when this code is executed in a bootstraped image"
	
	args := selector numArgs.

	args = 0
		ifTrue: [ ^ selector ].

	selector isBinary ifTrue: [ ^ String streamContents: 
		[ :s | s nextPutAll: selector; nextPutAll: ' anObject' ] ].

	^ String
		streamContents: [ :stream | 
					keywords := selector keywords.
					1 to: args do: [ :argumentNumber | 
						stream
							nextPutAll: (keywords at: argumentNumber);
							space;
							nextPutAll: 'arg';
							nextPutAll: argumentNumber asString;
							space ] ] 
]

{ #category : #accessing }
TaSequence >> selectors [
	^ members flatCollect: #selectors
]

{ #category : #accessing }
TaSequence >> slots [
	^ (members flatCollect: #slots)
		removeDuplicates;
		yourself
]

{ #category : #accessing }
TaSequence >> sourceCodeAt: selector [
	(self isConflictingSelector: selector) 
		ifTrue: [ ^ self sourceCodeForConflictFor: selector ].

	^ (self memberForSelector: selector) sourceCodeAt: selector
]

{ #category : #accessing }
TaSequence >> sourceCodeForConflictFor: selector [

	"I return the method body for a conflicting selector"

	^ String
		streamContents: [ :s | 
			s
				nextPutAll: (self selectorForConflict: selector);
				cr;
				tab;
				nextPutAll: 'self traitConflict' ]
]

{ #category : #printing }
TaSequence >> traitCompositionExpression [
	self isEmpty
		ifTrue: [ ^ '{}' ].

	^ {members first traitCompositionExpression}
		, (members allButFirst collect: #traitCompositionExpressionWithParens) joinUsing: ' + '
]

{ #category : #querying }
TaSequence >> traitDefining: aSelector [
	^ (self memberForSelector: aSelector) traitDefining: aSelector
]

{ #category : #querying }
TaSequence >> traits [
	^ members flatCollect: #traits
]

{ #category : #validations }
TaSequence >> validateDuplications: anElement [
	(members noneSatisfy: [ :e | e = anElement ])
		ifFalse:[self error:'Could not include the same traits twice'].
]

{ #category : #validations }
TaSequence >> validateMethods: aTrait [
	| newMethods myMethods|
	myMethods := self methods.
	newMethods := aTrait methods reject: [ :e | myMethods includes: e ].

	(self selectors
		noneSatisfy: [ :e | newMethods anySatisfy: [ :x | x selector = e ] ])
		ifFalse: [ self error: 'The added trait duplicates an existing selector' ]
]

{ #category : #validations }
TaSequence >> validateSlots: anElement [
	self slots
		do: [ :e | 
			anElement slots
				do: [ :other | 
					(e name = other name and: [ e definingClass ~= other definingClass ])
						ifTrue: [ self error: 'The added trait duplicates an existing slot ' , e name printString , '. Duplication between ' , e definingClass asString , ' and ' , other definingClass asString ] ] ]
]

{ #category : #precedence }
TaSequence >> withPrecedenceOf: aTrait [ 
	
	| aTraitComposition |
	aTraitComposition := aTrait asTraitComposition.
	
	(members includes: aTraitComposition)
		ifFalse: [ self error: 'A trait cannot be marked with precedence if it is not in the sequence' ].
		
	^ (TaPrecedenceComposition withAll: members)
			preferedTrait: aTraitComposition;
			yourself
]

{ #category : #operations }
TaSequence >> without: aTrait [
	| newMembers |
	newMembers := members collect:[:e | e without: aTrait] thenSelect:#isNotNil.
	
	newMembers ifEmpty: [ ^ nil ].
	
	^ self class withAll: newMembers.
]
