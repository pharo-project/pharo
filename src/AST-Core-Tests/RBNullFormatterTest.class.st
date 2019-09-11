"
SUnit tests for RBNullFormatter
"
Class {
	#name : #RBNullFormatterTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Formatter'
}

{ #category : #helpers }
RBNullFormatterTest >> parseMethod: aString [ 

	^ RBParser parseMethod: aString
]

{ #category : #tests }
RBNullFormatterTest >> testGivenAMethodNodeWhenSourceAvailableThenSourceReturnedAsResultOfFormatting [
	| ast sourceCode |
	sourceCode := (Collection class>>#with:with:) sourceCode.
	ast := self parseMethod: sourceCode.
	self 
		assert: (RBNullFormatter new format: ast)
		equals: sourceCode
]

{ #category : #tests }
RBNullFormatterTest >> testGivenAMethodNodeWhenSourceNotAvailableThenSelectorWithWarningMessageReturnedAsResultOfFormatting [
	| ast sourceCode |
	sourceCode := (Collection class>>#with:with:) sourceCode.
	ast := self parseMethod: sourceCode.
	ast source: nil.
	self 
		assert: (RBNullFormatter new format: ast)
		equals: 'with:with:
	''no source and no formatter available!'''
]
