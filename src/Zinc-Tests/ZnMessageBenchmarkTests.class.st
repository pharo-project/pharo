Class {
	#name : #ZnMessageBenchmarkTests,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnMessageBenchmarkTests >> testRequests [
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
ZnMessageBenchmarkTests >> testResponses [
	ZnMessageBenchmark responses do: [ :each |
		| bench |
		bench := ZnMessageBenchmark new.
		bench perform: each.
		self assert: (bench message isKindOf: ZnResponse).
		bench writeRepresentation.
		self assert: (bench read isKindOf: ZnResponse).
		self assert: bench read equals: bench message ]
		
	
]
