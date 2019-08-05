Class {
	#name : #ZnMessageBenchmarkTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnMessageBenchmarkTest >> testRequests [
	ZnMessageBenchmark requests do: [ :each |
		| bench |
		bench := ZnMessageBenchmark new.
		bench perform: each.
		self assert: (bench message isKindOf: ZnRequest).
		bench writeRepresentation.
		self assert: (bench read isKindOf: ZnRequest).
		self assert: bench read equals: bench message ]
		
	
]

{ #category : #testing }
ZnMessageBenchmarkTest >> testResponses [
	ZnMessageBenchmark responses do: [ :each |
		| bench |
		bench := ZnMessageBenchmark new.
		bench perform: each.
		self assert: (bench message isKindOf: ZnResponse).
		bench writeRepresentation.
		self assert: (bench read isKindOf: ZnResponse).
		self assert: bench read equals: bench message ]
		
	
]
