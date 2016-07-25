printing
printElementsOn: stream
	stream
		nextPut: $(;
		nextPut: $#;
		print: self size;
		nextPut: $)