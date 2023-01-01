"
RBPatternMethodNode is an RBMethodNode that will match other method nodes without their selectors being equal. 

Instance Variables:
	isList	<Boolean>	are we matching each keyword or matching all keywords together (e.g., `keyword1: would match a one-argument method whereas `@keywords: would match 0 or more arguments)


"
Class {
	#name : #RBPatternMethodNode,
	#superclass : #RBMethodNode,
	#instVars : [
		'isList'
	],
	#category : #'AST-Core-Pattern'
}

{ #category : #'instance creation' }
RBPatternMethodNode class >> selector: aSelector keywordsPositions: positionList arguments: valueNodes [
	^(aSelector isPatternVariable)
		ifTrue: [super selector: aSelector keywordsPositions: positionList arguments: valueNodes]
		ifFalse:
			[RBMethodNode selector: aSelector keywordsPositions: positionList arguments: valueNodes]
]

{ #category : #matching }
RBPatternMethodNode >> copyInContext: aDictionary [
	| keywordSelector |
	keywordSelector := self isSelectorList
		ifTrue: [ aDictionary at: self selectorParts first ]
		ifFalse: [ ('' join:(self selectorParts collect: [ :each | aDictionary at: each ])) asSymbol  ].
	^ RBMethodNode new
		selector: keywordSelector;
		arguments: (self copyList: self arguments inContext: aDictionary);
		pragmas: (self pragmas isEmpty
			ifTrue: [ aDictionary at: '-pragmas-' ifAbsent: [ #() ] ]
			ifFalse: [ self copyList: self pragmas inContext: aDictionary ]);
		body: (self body copyInContext: aDictionary);
		source: (aDictionary at: '-source-');
		yourself
]

{ #category : #'testing - matching' }
RBPatternMethodNode >> isPatternNode [
	^true
]

{ #category : #testing }
RBPatternMethodNode >> isSelectorList [
	^isList
]

{ #category : #matching }
RBPatternMethodNode >> match: aNode inContext: aDictionary [
	aNode class = self matchingClass
		ifFalse: [ ^ false ].
	aDictionary at: '-source-' put: aNode source.
	self isSelectorList ifTrue: [
		^ (aDictionary at:  self selectorParts first ifAbsentPut: [ aNode selector ]) = aNode selector
			and: [ (arguments isEmpty or: [(aDictionary at: arguments first ifAbsentPut: [ aNode arguments ]) = aNode arguments])
			and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
			and: [ body match: aNode body inContext: aDictionary ] ] ] ].
	^(self matchArgumentsAgainst: aNode inContext: aDictionary)
		and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
		and: [ body match: aNode body inContext: aDictionary ] ]
]

{ #category : #matching }
RBPatternMethodNode >> matchArgumentsAgainst: aNode inContext: aDictionary [
	self arguments size = aNode arguments size
		ifFalse: [ ^ false ].
	(self matchSelectorAgainst: aNode inContext: aDictionary)
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #matching }
RBPatternMethodNode >> matchSelectorAgainst: aNode inContext: aDictionary [
	self selectorParts with: aNode selectorParts do: [ :first :second |
		| keyword |
		keyword := aDictionary
			at: first
			ifAbsentPut: [
				first isPatternVariable
					ifTrue: [ second ]
					ifFalse: [ first ] ].
		keyword = second
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #private }
RBPatternMethodNode >> matchingClass [
	^RBMethodNode
]

{ #category : #initialization }
RBPatternMethodNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	super selector: aSymbol keywordsPositions: positionList arguments: valueNodes.
	isList := (aSymbol keywords first value at: 2) == self listCharacter
]
