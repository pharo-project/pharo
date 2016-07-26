exceptions
onDNU: selector do: handleBlock
	"Catch MessageNotUnderstood exceptions but only those of the given selector (DNU stands for doesNotUnderstand:)"

	^ self on: MessageNotUnderstood do: [:exception |
		exception message selector = selector
			ifTrue: [handleBlock cull: exception]
			ifFalse: [exception pass]
	  ]