"
SUnit tests for RBPatternParser.
RBPatternParser needs some extra tests not covered by RBPatternTest for its extended syntax
(pattern variables and pattern blocks)
"
Class {
	#name : #RBPatternParserTest,
	#superclass : #RBParserTest,
	#category : #'AST-Tests-Core'
}

{ #category : #accessing }
RBPatternParserTest class >> parserClass [
	^ RBPatternParser
]

{ #category : #'tests parsing' }
RBPatternParserTest >> testParseFaultyPatternBlock [
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

{ #category : #'tests parsing' }
RBPatternParserTest >> testPatternBlockNode1 [
	| searchPattern tree |
	searchPattern := RBPatternParser
		parseExpression: '(`a = `b) `{`a name size = `b name size}'.
	tree := RBParser parseExpression: 'a = c'.
	self assert: (searchPattern match: tree inContext: Dictionary new)
]

{ #category : #'tests parsing' }
RBPatternParserTest >> testPatternBlockNode2 [
	| searchPattern tree |
	searchPattern := RBPatternParser
		parseExpression: '(`a = `b) `{`a name anySatisfy: [ :e | `b name includes: e]}'.
	tree := RBParser parseExpression: 'a = a1'.
	self assert: (searchPattern match: tree inContext: Dictionary new)
]
