evaluating
value

	| strongReceiver |
	
	strongReceiver := self receiver.

	^ arguments isNil
		ifTrue: [(self ensureReceiver: strongReceiver)
				ifTrue: [strongReceiver perform: selector] ifFalse: []]
		ifFalse: [(self ensureReceiverAndArguments: strongReceiver)
				ifTrue: [strongReceiver
						perform: selector
						withArguments: (Array withAll: arguments)] ifFalse: []].
