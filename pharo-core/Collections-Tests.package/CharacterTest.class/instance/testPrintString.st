tests
testPrintString
	self assert: $a printString = '$a'.
	self assert: $5 printString = '$5'.
	self assert: $@ printString = '$@'.

	self assert: Character cr printString = 'Character cr'.
	self assert: Character lf printString = 'Character lf'.
	self assert: Character space printString = 'Character space'.

	self assert: (Character value: 0) printString = 'Character null'.
	self assert: (Character value: 17) printString = 'Character value: 17'.