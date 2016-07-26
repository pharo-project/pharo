instruction decoding
methodReturnReceiver
	"Simulate the action of a 'return receiver' bytecode. This corresponds to
	 the source expression '^self'."

	^self return: self receiver from: self methodReturnContext