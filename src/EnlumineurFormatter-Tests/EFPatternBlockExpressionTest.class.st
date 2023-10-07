Class {
	#name : 'EFPatternBlockExpressionTest',
	#superclass : 'EFBlockExpressionTest',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'hooks' }
EFPatternBlockExpressionTest >> closeBracket [
	^'}'
]

{ #category : 'hooks' }
EFPatternBlockExpressionTest >> openBracket [
	^'`{'
]

{ #category : 'hooks' }
EFPatternBlockExpressionTest >> parserClass [
	^ RBPatternParser
]
