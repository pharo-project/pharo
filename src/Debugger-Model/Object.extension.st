Extension { #name : 'Object' }

{ #category : '*Debugger-Model' }
Object class >> canonicalArgumentName [
	"Imagine you write a method using this object (self) as argument, I return how you would likely
	name that argument.
	Used by DynamicMessageImplementor in the debugger to create a template method when an execution
	calls a method that does not exist."
	| prefix |
	prefix := self name first isVowel
		ifTrue: [ 'an' ]
		ifFalse: [ 'a' ].
	^ prefix, self name
]
