"
Floating point types are imprecise. Using the operators = or ~= might not yield the expected result due to internal rounding differences.
"
Class {
	#name : 'ReFloatEqualityComparisonRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReFloatEqualityComparisonRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReFloatEqualityComparisonRule class >> ruleName [
	^ 'Float equality comparison'
]

{ #category : 'accessing' }
ReFloatEqualityComparisonRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'FloatEqualityComparisonRule'
]

{ #category : 'hooks' }
ReFloatEqualityComparisonRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`#floatLiteral') value isFloat
]

{ #category : 'initialization' }
ReFloatEqualityComparisonRule >> initialize [
	super initialize.
	self matchesAny: #(
			'`#floatLiteral = `@expr'
			'`#floatLiteral ~= `@expr'
			'`@expr = `#floatLiteral'
			'`@expr ~= `#floatLiteral' )
]
