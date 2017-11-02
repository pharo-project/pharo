"
Floating point types are imprecise. Using the operators = or ~= might not yield the expected result due to internal rounding differences.
"
Class {
	#name : #RBFloatEqualityComparisonRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBFloatEqualityComparisonRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'FloatEqualityComparisonRule'
]

{ #category : #accessing }
RBFloatEqualityComparisonRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBFloatEqualityComparisonRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`{ :node | node isLiteralNode and: [ node value isFloat ] } = `@expr'
			'`{ :node | node isLiteralNode and: [ node value isFloat ] } ~= `@expr'
			'`@expr = `{ :node | node isLiteralNode and: [ node value isFloat ] }'
			'`@expr ~= `{ :node | node isLiteralNode and: [ node value isFloat ] }' )
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBFloatEqualityComparisonRule >> name [
	^ 'Float equality comparison'
]
