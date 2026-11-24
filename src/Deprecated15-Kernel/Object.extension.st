Extension { #name : 'Object' }

{ #category : '*Deprecated15-Kernel' }
Object >> crTrace: aString [

	self
		deprecated: 'Use #newLineTrace: instead'
		in: #Pharo15
		transformWith: '`@rcv crTrace: `@arg' -> '`@rcv newLineTrace: `@arg'.

	self newLineTrace: aString
]

{ #category : '*Deprecated15-Kernel' }
Object >> traceCrTab: anObject [

	self
		deprecated: 'Use #traceNewLineTab: instead.'
		in: #Pharo15
		transformWith: '`@rcv traceCrTab: `@arg' -> '`@rcv traceNewLineTab: `@arg'.

	self traceNewLineTab: anObject
]
