"
Checks for unconditional recursion that might cause the image to hang when executed.
"
Class {
	#name : #RBUnconditionalRecursionRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBUnconditionalRecursionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnconditionalRecursionRule'
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBUnconditionalRecursionRule >> initialize [
	super initialize.
	self matcher
		matchesMethod: '`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
		do: [ :node :answer | 
			| index |
			index := node body statements
				findFirst: [ :each | each isMessage and: [ each selector = node selector ] ].
			((node body statements copyFrom: 1 to: index)
				anySatisfy: [ :each | each containsReturn ])
					ifTrue: [ answer ]
					ifFalse: [ node ] ]
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> name [
	^ 'Unconditional recursion'
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> severity [
	^ #error
]
