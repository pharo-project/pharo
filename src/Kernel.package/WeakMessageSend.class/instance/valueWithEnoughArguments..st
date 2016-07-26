evaluating
valueWithEnoughArguments: anArray
	"call the selector with enough arguments from arguments and anArray"
	| args strongReceiver |
	
	strongReceiver := self receiver.
	
	(self ensureReceiverAndArguments: strongReceiver) ifFalse: [ ^nil ].
	args := Array new: selector numArgs.
	args replaceFrom: 1
		to: ( arguments size min: args size)
		with: arguments
		startingAt: 1.
	args size > arguments size ifTrue: [
		args replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: args size)
			with: anArray
			startingAt: 1.
	].
	^ strongReceiver perform: selector withArguments: args
