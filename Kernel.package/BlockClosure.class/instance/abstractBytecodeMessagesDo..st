scanning
abstractBytecodeMessagesDo: aBlock
	"Evaluate aBlock with the sequence of abstract bytecodes in the receiver."
	self method
		abstractBytecodeMessagesFrom: startpc
		to: self endPC
		do: aBlock

	"| msgs |
	 msgs := OrderedCollection new.
	 (SortedCollection sortBlock: [:a :b| a compare: b caseSensitive: false]) sortBlock
		abstractBytecodeMessagesDo: [:msg| msgs add: msg selector].
	 msgs"