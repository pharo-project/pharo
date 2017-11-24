"
Checks for unconditional recursion that might cause the image to hang when executed.
"
Class {
	#name : #RBUnconditionalRecursionRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBUnconditionalRecursionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnconditionalRecursionRule'
]

{ #category : #hooks }
RBUnconditionalRecursionRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.before') noneSatisfy: #containsReturn
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBUnconditionalRecursionRule >> initialize [
	super initialize.
	self addMatchingMethod:
		'`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
		
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> name [
	^ 'Unconditional recursion'
]

{ #category : #accessing }
RBUnconditionalRecursionRule >> severity [
	^ #error
]
