evaluating
valueWithArguments: anArray

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	^ strongReceiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)