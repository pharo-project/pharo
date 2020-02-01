"
Checks for unconditional recursion that might cause the image to hang when executed.
"
Class {
	#name : #ReUnconditionalRecursionRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUnconditionalRecursionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnconditionalRecursionRule'
]

{ #category : #hooks }
ReUnconditionalRecursionRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.before') noneSatisfy: #containsReturn
]

{ #category : #accessing }
ReUnconditionalRecursionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
ReUnconditionalRecursionRule >> initialize [
	super initialize.
	self addMatchingMethod:
		'`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
		
]

{ #category : #accessing }
ReUnconditionalRecursionRule >> name [
	^ 'Unconditional recursion'
]

{ #category : #accessing }
ReUnconditionalRecursionRule >> severity [
	^ #error
]
