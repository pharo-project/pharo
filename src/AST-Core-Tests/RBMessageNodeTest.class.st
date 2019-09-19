Class {
	#name : #RBMessageNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #testing }
RBMessageNodeTest >> testArgumentPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #('2').
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages [
	| tree message |
	tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('x + 2' '(y foo: 3)')
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages1 [
	| tree message |
	tree := self parseMethod: 'test self between: 2 and: 3'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('2' '3').
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages2 [

		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y').
		
		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y' 'zzz')
]

{ #category : #testing }
RBMessageNodeTest >> testArgumentPartsForUnaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 foo '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #().
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywords [
		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y'.
		
		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y and: zzz'
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywordsAndComplexArgs [
		| tree message |
		tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x + 2 and: (y foo: 3)'.
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorAndArgumentNamesForSimpleKeywords [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit: 5'
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorAndArgumentNamesForUnary [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit'
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorInterval [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 14).
		tree := self parseMethod: 'test self doit: #nice with: 5'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 27).
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message selectorParts equals: #(#+).
		
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorPartsForKeywordMessages [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit:).
		
		tree := self parseMethod: 'test self between: 0 and: 5 '.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#between: #and:).
		
]

{ #category : #testing }
RBMessageNodeTest >> testSelectorPartsForUnaryMessages [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit).
		
		
]
