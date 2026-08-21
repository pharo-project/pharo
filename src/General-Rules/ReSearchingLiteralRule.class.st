"
Checks for repeated literal equality tests that should rather be implemented as a search in a literal collection.

For example, the code:
    value = 1 or: [value = 2 or: [value = 3]]
    
Can be improved to:
    #(1 2 3) includes: value
"
Class {
	#name : 'ReSearchingLiteralRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReSearchingLiteralRule class >> groupSelector [

	^ #optimizationGroup
]

{ #category : 'accessing' }
ReSearchingLiteralRule class >> rationale [

	^ 'Using the includes: method on a literal array is more readable and efficient than chaining multiple equality tests with or:'
]

{ #category : 'accessing' }
ReSearchingLiteralRule class >> ruleName [
	^ 'Uses or instead of a searching literal'
]

{ #category : 'accessing' }
ReSearchingLiteralRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SearchingLiteralRule'
]

{ #category : 'initialization' }
ReSearchingLiteralRule >> initialize [

	super initialize.
	
	self
		replace: '``@object = `#literal1 or: [``@object = `#literal2]'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '``@object == `#literal1 or: [``@object == `#literal2]'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '`#literal1 = ``@object or: [`#literal2 = ``@object]'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '`#literal1 == ``@object or: [`#literal2 == ``@object]'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '(``@object = `#literal1) | (``@object = `#literal2)'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '(``@object == `#literal1) | (``@object == `#literal2)'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '(`#literal1 = ``@object) | (`#literal2 = ``@object)'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace: '(`#literal1 == ``@object) | (`#literal2 == ``@object)'
		with: '#(`#literal1 `#literal2) includes: ``@object'.

	self
		replace:
		'``@object = `#literal1 or: [``@object = `#literal2 or: [``@object = `#literal3]]'
		with: '#(`#literal1 `#literal2 `#literal3) includes: ``@object'.

	self
		replace:
		'``@object == `#literal1 or: [``@object == `#literal2 or: [``@object == `#literal3]]'
		with: '#(`#literal1 `#literal2 `#literal3) includes: ``@object'.

	self
		replace:
		'`#literal1 = ``@object or: [`#literal2 = ``@object or: [`#literal3 = ``@object]]'
		with: '#(`#literal1 `#literal2 `#literal3) includes: ``@object'.

	self
		replace:
		'`#literal1 == ``@object or: [`#literal2 == ``@object or: [`#literal3 == ``@object]]'
		with: '#(`#literal1 `#literal2 `#literal3) includes: ``@object'.
		
	self
		replace: '``@object = `#literal1 or: [#() = ``@object | (#() == ``@object)]'
		with: '#(`#literal1 #()) includes: ``@object'.
]
