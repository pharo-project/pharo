testing
testRequests
	ZnMessageBenchmark requests do: [ :each |
		| bench |
		bench := ZnMessageBenchmark new.
		bench perform: each.
		self assert: (bench message isKindOf: ZnRequest).
		bench writeRepresentation.
		self assert: (bench read isKindOf: ZnRequest).
		self assert: bench read equals: bench message ]
		
	