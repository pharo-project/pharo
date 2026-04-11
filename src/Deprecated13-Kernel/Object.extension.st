Extension { #name : 'Object' }

{ #category : '*Deprecated13-Kernel' }
Object >> explicitRequirement [
	"If one of the superclasses can perform the selector, we execute the method of that class, otherwise, the explicit requirement error is thrown"

	<debuggerCompleteToSender>
	| sender |
	
	"self deprecated: 'This method will be removed in the future version of Pharo. See https://github.com/pharo-project/pharo/issues/15507 for details'.
	I comment it out because it produces a lot of noise in the logs. We will have to take 
	a decision on this."

	sender := thisContext sender.
	[ sender return: (self perform: sender selector withArguments: sender arguments inSuperclass: sender methodClass superclass) ]
		on: MessageNotUnderstood
		do: [ self error: 'Explicitly required method' ]
]
