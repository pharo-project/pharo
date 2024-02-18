Extension { #name : 'Object' }

{ #category : '*Deprecated12' }
Object >> asOrderedCollection [

	self
		deprecated:
		'The usage of this method is not recommended. We want to have a smaller Object api. We will remove this method in the next Pharo version.'
"Automatic transform removed. See https://github.com/pharo-project/pharo/issues/14197"
		"transformWith: '`@receiver asOrderedCollection' -> 'OrderedCollection with: `@receiver'".
	^ OrderedCollection with: self
]

{ #category : '*Deprecated12' }
Object >> nominallyUnsent: aSelectorSymbol [
	"From within the body of a method which is not formally sent within the system, but which you intend to have remain in the system (for potential manual invocation, or for documentation, or perhaps because it's sent by commented-out-code that you anticipate uncommenting out someday, send this message, with the selector itself as the argument.

This will serve two purposes:

	(1)  The method will not be returned by searches for unsent selectors (because it, in a manner of speaking, sends itself).
	(2)	You can locate all such methods by browsing senders of #nominallyUnsent:"

	self deprecated: 'Will be remove without replacement'.

	false ifTrue: [ "So that this method itself will appear to be sent"
		self flag: #nominallyUnsent: ]
]
