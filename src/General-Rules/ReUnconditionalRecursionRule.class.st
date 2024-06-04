"
Checks for unconditional recursion that might cause the image to hang when executed.
"
Class {
	#name : 'ReUnconditionalRecursionRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing' }
ReUnconditionalRecursionRule class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReUnconditionalRecursionRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReUnconditionalRecursionRule class >> ruleName [
	^ 'Unconditional recursion'
]

{ #category : 'accessing' }
ReUnconditionalRecursionRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReUnconditionalRecursionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UnconditionalRecursionRule'
]

{ #category : 'hooks' }
ReUnconditionalRecursionRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.before') noneSatisfy: #containsReturn
]

{ #category : 'initialization' }
ReUnconditionalRecursionRule >> initialize [
	super initialize.
	self addMatchingMethod:
		'`@message: `@args
			| `@temps |
			`@.before.
			self `@message: `@args.
			`@.after'
]
