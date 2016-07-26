bytecode decoding
popIntoReceiverVariable: offset 
	varIndex = offset 
		ifTrue: [ found := true ]
		ifFalse: [ self reset ]