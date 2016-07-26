private-extensions-flexecutionbenchmarks
benchs
	<flExtensionOf: #FLExecutionBenchmarks>
	<flExtensionClassSide>
	(self class selectorsInProtocol: 'executing')
		do: [:m |
			Transcript 
				cr; 
				show: m printString;
				show: '->'.
			[self perform: m] bench printOn: Transcript ].
	Transcript flush