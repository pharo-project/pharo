evaluating
value: anObject

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].
	
	^ strongReceiver
		perform: selector 
		with: anObject