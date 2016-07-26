*GT-SpotterExtensions-Core
spotterForBytecodesFor: aStep
	<spotterOrder: 15>
	aStep listProcessor
		title: 'Bytecode';
		allCandidates: [ self symbolicBytecodes ];
		itemName: #printString;
		filter: GTFilterSubstring