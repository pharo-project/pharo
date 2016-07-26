private
showPassword: aBoolean

	(showPassword := aBoolean)
		ifTrue: [ self beDecrypted ]
		ifFalse: [ self beEncrypted ].
	self changed: #showPassword