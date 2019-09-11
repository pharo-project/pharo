Class {
	#name : #BIPatternBlockExpressionTest,
	#superclass : #BIBlockExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #hooks }
BIPatternBlockExpressionTest >> closeBracket [
	^'}'
]

{ #category : #hooks }
BIPatternBlockExpressionTest >> openBracket [
	^'`{'
]

{ #category : #hooks }
BIPatternBlockExpressionTest >> parser [
	^ RBPatternParser
]
