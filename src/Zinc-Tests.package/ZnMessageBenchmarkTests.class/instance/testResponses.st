testing
testResponses
	ZnMessageBenchmark responses do: [ :each |
		| bench |
		bench := ZnMessageBenchmark new.
		bench perform: each.
		self assert: (bench message isKindOf: ZnResponse).
		bench writeRepresentation.
		self assert: (bench read isKindOf: ZnResponse).
		self assert: bench read equals: bench message ]
		
	