"
Checks for common ifTrue: returns that could be simplified using a boolean expression.
"
Class {
	#name : #RBIfTrueReturnsRule,
	#superclass : #RBParseTreeLintRule,
	#category : #'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBIfTrueReturnsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'IfTrueReturnsRule'
]

{ #category : #accessing }
RBIfTrueReturnsRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBIfTrueReturnsRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'| `@temps | ``@.Statements. ``@object ifTrue: [^``@value1]. ^``@value2' 
			'| `@temps | ``@.Statements. ``@object ifFalse: [^``@value1]. ^``@value2' )
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| condition |
					condition := (node statements at: node statements size - 1) arguments first 
						body statements 
						last value.	"``@value1"
					((condition isLiteralNode
						and: [ #(true false) includes: condition value ]) 
						or: [ 
							condition := 	node statements last value.
							condition isLiteralNode and: [ #(true false) includes: condition value ] ]) 
								ifTrue: [ node ]
								ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBIfTrueReturnsRule >> name [
	^ 'ifTrue:/ifFalse: returns instead of and:/or:''s'
]
