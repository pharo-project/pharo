encryption
encrypted: aBoolean

	aBoolean
		ifTrue: [ self beEncrypted ]
		ifFalse: [ self beDecrypted ]