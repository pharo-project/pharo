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
