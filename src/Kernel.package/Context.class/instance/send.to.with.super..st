controlling
"protocol: controlling"
send: selector to: aReceiver with: arguments super: superFlag 
	"Simulate the action of sending a message with selector, selector, and 
	arguments, args, to receiver. The argument, superFlag, tells whether the 
	receiver of the message was specified with 'super' in the source method."

	| class aMethod value context |
	class := superFlag
			ifTrue: [(self method literalAt: self method numLiterals) value superclass]
			ifFalse: [self objectClass: aReceiver].
	aMethod := class lookupSelector: selector.
	aMethod == nil ifTrue: [ 
		^ self 
			send: #doesNotUnderstand:
			to: aReceiver
			with: (Array with: (Message selector: selector arguments: arguments))
			super: superFlag ].
	
	value := self tryPrimitiveFor: aMethod receiver: aReceiver args: arguments.

	"primitive runs without failure?"
	(self isFailToken: value) ifFalse: [^ value].
	
	(selector == #doesNotUnderstand: and: [ (class canUnderstand: #doesNotUnderstand: ) not ]) 
		ifTrue: [
			^self error: 'Simulated message ', (arguments at: 1) selector, ' not understood'].
	
	"failure.. lets activate the method"
	context := self activateMethod: aMethod withArgs: arguments receiver: aReceiver class: class.
	
	"check if activated method handles the error code (a first bytecode will be store into temp)"
	"long store temp" 
	 (context method at: context pc ) = 129 
		ifTrue: [ context at: context stackPtr put: value last ].
	^ context