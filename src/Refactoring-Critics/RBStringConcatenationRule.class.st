"
Check for string concatenation inside some iteration message. Since string concatenation is O(n^2), it is better to use streaming since it is O(n) - assuming that n is large enough. As a general principal avoid , since the receiver is copied. Therefore chaining , messages will lead to multiple useless copies of the receiver. 

Instead of writing
	| string | 
	string := String new.
	#(1 2 3) do: [ :each |
		string := string, each asString].
	^ string

Write, it is much more efficient.

	String streamContents: [:s | 
		#(1 2 3)  do: [:each | s nextPutAll: each asString]]
	
or more concisely...
	'''' join: {1. 1+1. 3}	

"
Class {
	#name : #RBStringConcatenationRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBStringConcatenationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'StringConcatenationRule'
]

{ #category : #accessing }
RBStringConcatenationRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBStringConcatenationRule >> initialize [
	| concatenationMatcher |
	super initialize.
	concatenationMatcher := RBParseTreeSearcher new.
	concatenationMatcher 
		matches: '`@receiver , `@argument'
		do: [ :node :answer | true ].
	self matcher 
		matchesAnyOf: #(
			'``@collection do: ``@argument' 
			'``@collection do: ``@argument1 separatedBy: ``@argument2' 
			'``@start to: ``@stop do: ``@argument' 
			'``@collection detect: ``@argument' 
			'``@collection detect: ``@argument1 ifNone: ``@argument2' 
			'``@collection select: ``@argument'
			'``@collection reject: ``@argument' 
			'``@collection inject: ``@value into: ``@argument'
			'``@collection anySatisfy: ``@argument'
			'``@collection allSatisfy: ``@argument'
			'``@collection noneSatisfy: ``@argument' )
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(node arguments 
						detect: [ :each | each isBlock and: [ concatenationMatcher executeTree: each initialAnswer: false ] ]
						ifNone: [ nil ]) notNil 
							ifTrue: [ node ]
							ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBStringConcatenationRule >> name [
	^ 'String concatenation instead of streams'
]

{ #category : #accessing }
RBStringConcatenationRule >> rationale [
	^ 'Check for code using string concatenation inside some iteration message.'
]
