keymapping protocol
keyText: aString if: aBoolean
	aBoolean ifFalse: [ ^ self ].
	self keyText: aString