Extension { #name : 'Object' }

{ #category : '*Deprecated13' }
Object >> explicitRequirement [
	"If one of the superclasses can perform the selector, we execute the method of that class, otherwise, the explicit requirement error is thrown"

	<debuggerCompleteToSender>
	| sender |
	
	self deprecated: 'This method will be removed in the future version of Pharo. See https://github.com/pharo-project/pharo/issues/15507 for details'.

	sender := thisContext sender.
	[ sender return: (self perform: sender selector withArguments: sender arguments inSuperclass: sender methodClass superclass) ]
		on: MessageNotUnderstood
		do: [ self error: 'Explicitly required method' ]
]
