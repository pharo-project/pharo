instruction decoding
send: selector super: superFlag numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, 
	selector. The argument, superFlag, tells whether the receiver of the 
	message was specified with 'super' in the source method. The arguments 
	of the message are found in the top numArgs locations on the stack and 
	the receiver just below them."

	| currentReceiver arguments  |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | 
		arguments at: i put: self pop ].
	currentReceiver := self pop.
"	selector == #doPrimitive:method:receiver:args:
		ifTrue: [answer := receiver 
					doPrimitive: (arguments at: 1)
					method: (arguments at: 2)
					receiver: (arguments at: 3)
					args: (arguments at: 4).
				self push: answer.
				^self]. "
	QuickStep == self ifTrue: [
		QuickStep := nil.
		^ self quickSend: selector to: currentReceiver with: arguments super: superFlag].
	^ self send: selector to: currentReceiver with: arguments super: superFlag