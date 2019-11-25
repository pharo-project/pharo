Class {
	#name : #EFPatternBlockExpressionTest,
	#superclass : #EFBlockExpressionTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #hooks }
EFPatternBlockExpressionTest >> closeBracket [
	^'}'
]

{ #category : #hooks }
EFPatternBlockExpressionTest >> openBracket [
	^'`{'
]

{ #category : #hooks }
EFPatternBlockExpressionTest >> parserClass [
	^ RBPatternParser
]
