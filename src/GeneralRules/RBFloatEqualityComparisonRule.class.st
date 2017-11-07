"
Floating point types are imprecise. Using the operators = or ~= might not yield the expected result due to internal rounding differences.
"
Class {
	#name : #RBFloatEqualityComparisonRule,
	#superclass : #ReNodeMatchRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #accessing }
RBFloatEqualityComparisonRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'FloatEqualityComparisonRule'
]

{ #category : #hooks }
RBFloatEqualityComparisonRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`#floatLiteral') value isFloat
]

{ #category : #accessing }
RBFloatEqualityComparisonRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBFloatEqualityComparisonRule >> initialize [
	super initialize.
	self matchesAny: #(
			'`#floatLiteral = `@expr'
			'`#floatLiteral ~= `@expr'
			'`@expr = `#floatLiteral'
			'`@expr ~= `#floatLiteral' )
]

{ #category : #accessing }
RBFloatEqualityComparisonRule >> name [
	^ 'Float equality comparison'
]
