signatures
intcharstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (spAlien signedLongAt: 1)
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))