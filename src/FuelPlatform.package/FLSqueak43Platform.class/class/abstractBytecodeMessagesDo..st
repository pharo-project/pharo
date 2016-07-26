private-extensions-blockclosure
abstractBytecodeMessagesDo: aBlock
	<flExtensionOf: #BlockClosure isSource: true>
	^ 'abstractBytecodeMessagesDo: aBlock
	"Evaluate aBlock with the sequence of abstract bytecodes in the receiver."
	self method
		abstractBytecodeMessagesFrom: startpc
		to: self endPC
		do: aBlock'