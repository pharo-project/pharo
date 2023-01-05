"
RBPatternPragmaNode is an RBPragmaNode that is used by the tree searcher to
match pragma statements. Just like RBPatternMethodNode for method nodes.

Instance Variables:
	isList	<Boolean>	are we matching each keyword or matching all keywords together (e.g., `keyword1: would match a one-argument method whereas `@keywords: would match 0 or more arguments)

"
Class {
	#name : #RBPatternPragmaNode,
	#superclass : #RBPragmaNode,
	#instVars : [
		'isList'
	],
	#category : #'AST-Core-Pattern'
}

{ #category : #'instance creation' }
RBPatternPragmaNode class >> selector: aSelector keywordsPositions: positionList arguments: valueNodes [
	^(aSelector isPatternVariable)
		ifTrue: [ super selector: aSelector keywordsPositions: positionList arguments: valueNodes ]
		ifFalse: [ RBPragmaNode selector: aSelector keywordsPositions: positionList arguments: valueNodes ]
]

{ #category : #matching }
RBPatternPragmaNode >> copyInContext: aDictionary [
	| keywordSelector |
	keywordSelector := self isSelectorList
		ifTrue: [ aDictionary at: self selectorParts first ]
		ifFalse: [ ('' join: (self selectorParts collect: [ :each | aDictionary at: each ])) asSymbol ].
	^ RBPragmaNode
		selector: keywordSelector
		arguments: (self copyList: self arguments inContext: aDictionary)
]

{ #category : #'testing - matching' }
RBPatternPragmaNode >> isPatternNode [
	^ true
]

{ #category : #'testing - matching' }
RBPatternPragmaNode >> isSelectorList [
	^ isList
]

{ #category : #matching }
RBPatternPragmaNode >> match: aNode inContext: aDictionary [
	aNode class = self matchingClass
		ifFalse: [ ^ false ].
	self isSelectorList ifTrue: [
		^ (aDictionary at: self selectorParts first ifAbsentPut: [ aNode selector ]) = aNode selector
			and: [ (aDictionary at: arguments first ifAbsentPut: [ aNode arguments ]) = aNode arguments ] ].
	^ self matchArgumentsAgainst: aNode inContext: aDictionary
]

{ #category : #matching }
RBPatternPragmaNode >> matchArgumentsAgainst: aNode inContext: aDictionary [
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
RBPatternPragmaNode >> matchSelectorAgainst: aNode inContext: aDictionary [
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
RBPatternPragmaNode >> matchingClass [
	^ RBPragmaNode
]

{ #category : #initialization }
RBPatternPragmaNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList.
	isList := (aSymbol keywords first value at: 2) == self listCharacter
]

{ #category : #accessing }
RBPatternPragmaNode >> sentMessages [
	^ super sentMessages
		remove: self selector ifAbsent: [ ];
		yourself
]
