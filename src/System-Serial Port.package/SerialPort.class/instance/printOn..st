printing
printOn: aStream

	aStream
		nextPutAll: 'SerialPort(';
		nextPutAll:
			(port ifNil: ['closed'] ifNotNil: ['#', port printString]);
		nextPutAll: ', ';
		print: baudRate; nextPutAll: ' baud, ';
		print: dataBits; nextPutAll: ' bits, ';
		nextPutAll: (#('1.5' '1' '2') at: stopBitsType + 1); nextPutAll: ' stopbits, ';
		nextPutAll: (#('no' 'odd' 'even') at: parityType + 1); nextPutAll: ' parity)'.
