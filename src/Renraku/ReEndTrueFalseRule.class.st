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
	#name : #ReEndTrueFalseRule,
	#superclass : #ReNodeMatchRule,
	#category : #'Renraku-Rules-Rewrite'
}

{ #category : #accessing }
ReEndTrueFalseRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EndTrueFalseRule'
]

{ #category : #hooks }
ReEndTrueFalseRule >> afterCheck: aNode mappings: mappingDict [
	"after a node was match you can do a post-validation here on the node and the dictionalry of wildcars with matched nodes as values"

	^ mappingDict
		ifNil: [ | statement |
			statement := aNode arguments first body statements last.
			( statement isVariable and: [ statement = aNode arguments last body statements last ] ) not] 
		ifNotNil: [ true ]
]

{ #category : #accessing }
ReEndTrueFalseRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReEndTrueFalseRule >> initialize [

	super initialize.
	self matchesAny:
			#('`@object 
				ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
				ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' '`@object 
				ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
				ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' '`@object 
				ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
				ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' '`@object 
				ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
				ifTrue: [| `@temps2 | `.Statement. `@.Statement2]')
]

{ #category : #accessing }
ReEndTrueFalseRule >> name [
	^ 'Check for same statements at end of ifTrue:ifFalse: blocks'
]

{ #category : #accessing }
ReEndTrueFalseRule >> rationale [
	^ 'Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end.'
]

{ #category : #accessing }
ReEndTrueFalseRule >> severity [
	^ #information
]
