Extension { #name : 'SystemEnvironment' }

{ #category : '*Shout' }
SystemEnvironment >> hasBindingThatBeginsWith: aString [
	"Use the cached class and non-class names for better performance."

	| name searchBlock |
	(self pseudoVariables hasBindingThatBeginsWith: aString) ifTrue: [
		^ true ].
	searchBlock := [ :element |
	               (element beginsWith: aString)
		               ifTrue: [ 0 ]
		               ifFalse: [
			               aString < element
				               ifTrue: [ -1 ]
				               ifFalse: [ 1 ] ] ].
	name := self classNames findBinary: searchBlock ifNone: [ nil ].
	name ifNotNil: [ ^ true ].
	name := self nonClassNames findBinary: searchBlock ifNone: [ nil ].
	^ name isNotNil
]
