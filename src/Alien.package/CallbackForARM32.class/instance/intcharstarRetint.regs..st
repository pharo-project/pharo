signatures
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))