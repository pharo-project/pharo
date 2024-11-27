Class {
	#name : 'ASTMessageNodeTest',
	#superclass : 'ASTParseTreeTest',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
ASTMessageNodeTest >> testArgumentPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #('2')
]

{ #category : 'tests' }
ASTMessageNodeTest >> testArgumentPartsForKeywordMessages [
	| tree message |
	tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('x + 2' '(y foo: 3)')
]

{ #category : 'tests' }
ASTMessageNodeTest >> testArgumentPartsForKeywordMessages1 [
	| tree message |
	tree := self parseMethod: 'test self between: 2 and: 3'.
	message := tree sendNodes first.
	self assert: message argumentPartStrings asArray equals: #('2' '3')
]

{ #category : 'tests' }
ASTMessageNodeTest >> testArgumentPartsForKeywordMessages2 [

		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y').

		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message argumentPartStrings asArray equals: #('x' 'y' 'zzz')
]

{ #category : 'tests' }
ASTMessageNodeTest >> testArgumentPartsForUnaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 foo '.
	message := tree sendNodes first.
	self assert: message argumentPartStrings equals: #()
]

{ #category : 'tests' }
ASTMessageNodeTest >> testIntersectsInterval [

		| tree message |
		tree := self parseMethod: 'test self doit.'.
		self assert: 'test self doit.' size equals: 15.
		message := tree sendNodes first.
		self assert: (message intersectsInterval: (13 to: 14)).	" last 't' selected "
		self assert: (message intersectsInterval: (14 to: 13)).	" caret placed between 't' and '.' "
		self assert: (message intersectsInterval: (14 to: 15)).	" '.' selected "
		self deny:   (message intersectsInterval: (15 to: 14))	" caret placed after '.' "
]

{ #category : 'tests' }
ASTMessageNodeTest >> testIsInlineToDo [
	| messageNode |
	messageNode := (RBParser parseExpression: '1 to: 2 by: 2 do: [:each |]').
	self assert: messageNode isInlineToDo.
	messageNode := (RBParser parseExpression: '1 to: 2 by: nil do: [:each |]').
	self deny: messageNode isInlineToDo
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywords [
		| tree message |
		tree := self parseMethod: 'test self between: x and: y'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y'.

		tree := self parseMethod: 'test self between: x and: y and: zzz'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x and: y and: zzz'
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorAndArgumentNamesForComposedKeywordsAndComplexArgs [
		| tree message |
		tree := self parseMethod: 'test self between: x + 2 and: (y foo: 3)'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'between: x + 2 and: (y foo: 3)'
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorAndArgumentNamesForSimpleKeywords [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit: 5'
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorAndArgumentNamesForUnary [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorAndArgumentNames equals: 'doit'
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorInterval [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 14).
		tree := self parseMethod: 'test self doit: #nice with: 5'.
		message := tree sendNodes first.
		self assert: message selectorInterval equals: (11 to: 27)
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorPartsForBinaryMessages [
	| tree message |
	tree := self parseMethod: 'test 1 + 2 '.
	message := tree sendNodes first.
	self assert: message selectorParts equals: #(#+)
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorPartsForKeywordMessages [
		| tree message |
		tree := self parseMethod: 'test self doit: 5'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit:).

		tree := self parseMethod: 'test self between: 0 and: 5 '.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#between: #and:)
]

{ #category : 'tests' }
ASTMessageNodeTest >> testSelectorPartsForUnaryMessages [
		| tree message |
		tree := self parseMethod: 'test self doit'.
		message := tree sendNodes first.
		self assert: message selectorParts equals: #(#doit)
]

{ #category : 'tests' }
ASTMessageNodeTest >> testStart [
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
