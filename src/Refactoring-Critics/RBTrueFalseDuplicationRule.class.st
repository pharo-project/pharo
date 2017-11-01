"
Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end. Instead of having the same code in two places, you should move it outside the blocks.
"
Class {
	#name : #RBTrueFalseDuplicationRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBTrueFalseDuplicationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TrueFalseDuplication'
]

{ #category : #accessing }
RBTrueFalseDuplicationRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBTrueFalseDuplicationRule >> initialize [
	super initialize.
	self matcher
		matchesAnyOf: #(
			'`@object 
				ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
				ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' 
			'`@object 
				ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
				ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' 
			'`@object 
				ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
				ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' 
			'`@object 
				ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
				ifTrue: [| `@temps2 | `.Statement. `@.Statement2]') 
			do: [ :node :answer | 
				answer isNil 
					ifTrue: [
						| statement |
						statement := node arguments first body statements last.
						(statement isVariable and: [ statement = node arguments last body statements last ]) 
							ifFalse: [ node ]
							ifTrue: [ nil ] ]
					ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBTrueFalseDuplicationRule >> name [
	^ 'Check for same statements in ifTrue:ifFalse: blocks'
]

{ #category : #accessing }
RBTrueFalseDuplicationRule >> rationale [
	^ 'Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end.'
]

{ #category : #accessing }
RBTrueFalseDuplicationRule >> severity [
	^ #information
]
