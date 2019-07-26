"
Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end. While you might not originally write such code, as it is modified, it is easier to create such code. Instead of having the same code in two places, you should move it outside the blocks.

For example, 
test 
	ifTrue: [self foo. self bar ] 
	ifFalse: [ self foo. self baz ]
 is equivalent to: 

self foo.  
test 
	ifTrue: [ self bar ] 
	ifFalse: [ self baz ]
"
Class {
	#name : #RBEndTrueFalseRule,
	#superclass : #RBParseTreeLintRule,
	#category : #'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBEndTrueFalseRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EndTrueFalseRule'
]

{ #category : #accessing }
RBEndTrueFalseRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBEndTrueFalseRule >> initialize [

	super initialize.
	self matcher
		matchesAnyOf:
			#('`@object 
				ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
				ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' '`@object 
				ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
				ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' '`@object 
				ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
				ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' '`@object 
				ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
				ifTrue: [| `@temps2 | `.Statement. `@.Statement2]')
		do: [ :node :answer | 
			answer
				ifNil: [ | statement |

					statement := node arguments first body statements last.
					( statement isVariable and: [ statement = node arguments last body statements last ] )
						ifFalse: [ node ]
						ifTrue: [ nil ]
					]
				ifNotNil: [ answer ]
			]
]

{ #category : #accessing }
RBEndTrueFalseRule >> name [
	^ 'Check for same statements at end of ifTrue:ifFalse: blocks'
]

{ #category : #accessing }
RBEndTrueFalseRule >> rationale [
	^ 'Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end.'
]

{ #category : #accessing }
RBEndTrueFalseRule >> severity [
	^ #information
]
