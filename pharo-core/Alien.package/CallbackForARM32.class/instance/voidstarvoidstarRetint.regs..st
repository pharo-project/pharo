signatures
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))