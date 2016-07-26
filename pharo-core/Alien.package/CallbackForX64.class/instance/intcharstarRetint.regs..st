signatures
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))