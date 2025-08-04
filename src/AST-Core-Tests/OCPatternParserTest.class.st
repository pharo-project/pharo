"
SUnit tests for RBPatternParser.
RBPatternParser needs some extra tests not covered by RBPatternTest for its extended syntax
(pattern variables and pattern blocks)
"
Class {
	#name : 'OCPatternParserTest',
	#superclass : 'OCParserTest',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'accessing' }
OCPatternParserTest class >> parserClass [
	^ RBPatternParser
]

{ #category : 'tests - matching' }
OCPatternParserTest >> testMatchingPatternBlockNode1 [
	| searchPattern tree |
	searchPattern := RBPatternParser
		parseExpression: '(`a = `b) `{`a name size = `b name size}'.
	tree := self parseExpression: 'a = c'.
	self assert: (searchPattern match: tree inContext: Dictionary new)
]

{ #category : 'tests - matching' }
OCPatternParserTest >> testMatchingPatternBlockNode2 [
	| searchPattern tree |
	searchPattern := RBPatternParser
		parseExpression: '(`a = `b) `{`a name anySatisfy: [ :e | `b name includes: e]}'.
	tree := self parseExpression: 'a = a1'.
	self assert: (searchPattern match: tree inContext: Dictionary new)
]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testParseFaultyPatternBlock [
	| node patternBlockNoArgButBar patternBlockNoArgButColon unfinishedBlock varWithUnfinishedBlock |
	"parsing pattern block should mark this block as faulty, if it is unfinished no closing brace, or missing parts (colon but no arg,
	no arg but vertical bar)"
	patternBlockNoArgButBar := '`{ | }'.
	patternBlockNoArgButColon := '`{ : }'.
	unfinishedBlock := '`{ :  '.
	varWithUnfinishedBlock := '`sth `{ :  '.
	{patternBlockNoArgButBar. patternBlockNoArgButColon. unfinishedBlock. varWithUnfinishedBlock}
		do: [ :faultyExpression |
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isBlock.
			self assert: node isFaulty]
]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testPatternNotStringExpression [
	| searchPattern tree |
	searchPattern := RBPatternParser parseExpression: '`{ :node | node isString }'.

	tree := self parseExpression: 'a = a1'.

	self deny: (searchPattern match: tree inContext: Dictionary new).

]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testPatternStringExpression [
	| searchPattern tree |
	searchPattern := RBPatternParser parseExpression: '`{ :node | node isString }'.

	tree := self parseExpression: '''justAString'''.

	self assert: (searchPattern match: tree inContext: Dictionary new).

]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testPatternVariable [
	| searchPattern |
	searchPattern := RBPatternParser parseExpression: '`a'.
	self assert: searchPattern class = OCPatternVariableNode.
	self assert: searchPattern name equals: #'`a'
]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testPatternVariableSimpleVariable [
	| searchPattern |
	searchPattern := RBPatternParser parseExpression: '`a'.
	self assert: searchPattern class = OCPatternVariableNode.
	self assert: searchPattern name equals: #'`a'
]

{ #category : 'tests - parsing' }
OCPatternParserTest >> testPatternVariableSymbol [
	| searchPattern |
	searchPattern := RBPatternParser parseExpression: '`#aSymbol'.
	self assert: searchPattern class = OCPatternVariableNode.
	self assert: searchPattern name equals: #'`#aSymbol'
]
