signatures
voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))