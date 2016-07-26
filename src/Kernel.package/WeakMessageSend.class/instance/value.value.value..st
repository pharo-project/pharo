evaluating
value: anObject1 value: anObject2 value: anObject3

	| strongReceiver |
	
	strongReceiver := self receiver.

	(self ensureReceiver: strongReceiver) ifFalse: [ ^ nil ].
	
	^ strongReceiver
		perform: selector 
		with: anObject1
		with: anObject2
		with: anObject3