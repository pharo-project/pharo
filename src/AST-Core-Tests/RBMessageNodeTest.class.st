Class {
	#name : #RBMessageNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBMessageNodeTest >> testArgumentPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #('2')
]

{ #category : #tests }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages [
	| tree message |
	tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('x + 2' '(y foo: 3)')
]

{ #category : #tests }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages1 [
	| tree message |
	tree := self parseMethod: 'test self between: 2 and: 3'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('2' '3')
]

{ #category : #tests }
RBMessageNodeTest >> testArgumentPartsForKeywordMessages2 [

		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y').

		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y' 'zzz')
]

{ #category : #tests }
RBMessageNodeTest >> testArgumentPartsForUnaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 foo '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #()
]

{ #category : #tests }
RBMessageNodeTest >> testIntersectsInterval [
		| tree message |
		tree := self parseMethod: 'test self doit.'.
		message := tree sendNodes first.
		self assert: (message intersectsInterval: (13 to: 14)).	" last 't' selected "
		self assert: (message intersectsInterval: (14 to: 13)).	" caret placed between 't' and '.' "
		self assert: (message intersectsInterval: (14 to: 15)).	" '.' selected "
		self deny:   (message intersectsInterval: (15 to: 14))	" caret placed after '.' "
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywords [
		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y'.

		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y and: zzz'
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywordsAndComplexArgs [
		| tree message |
		tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x + 2 and: (y foo: 3)'
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorAndArgumentNamesForSimpleKeywords [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit: 5'
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorAndArgumentNamesForUnary [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit'
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorInterval [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 14).
		tree := self parseMethod: 'test self doit: #nice with: 5'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 27)
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message selectorParts equals: #(#+)
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorPartsForKeywordMessages [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit:).

		tree := self parseMethod: 'test self between: 0 and: 5 '.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#between: #and:)
]

{ #category : #tests }
RBMessageNodeTest >> testSelectorPartsForUnaryMessages [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit)
]

{ #category : #tests }
RBMessageNodeTest >> testStart [
	| tree message delta |

	tree := self parseMethod: 'test self doit.'.
	message := tree sendNodes first.

	self assert: message start equals: 6.
	self assert: message stop equals: 14.
	delta := message stop - message start.

	message start: 20.
	self assert: message start equals: 20.
	self assert: message stop equals: 20 + delta
]
