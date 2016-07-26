instruction decoding
send: selector super: superFlag numArgs: numArgs
	superFlag
		ifTrue: [irBuilder send: selector toSuperOf: self methodClass]
		ifFalse: [irBuilder send: selector]