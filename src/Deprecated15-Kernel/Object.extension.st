Extension { #name : 'Object' }

{ #category : '*Deprecated15-Kernel' }
Object >> crTrace [
	"Will be removed in Pharo 19."

	self
		deprecated: 'Use #newLineTrace instead.'
		on: '09/11/2026'
		in: #Pharo15
		transformWith: '`@receiver crTrace' -> '`@receiver newLineTrace'.
	self newLineTrace
]

{ #category : '*Deprecated15-Kernel' }
Object >> crTrace: aString [

	self deprecated: 'Use #newLineTrace: instead' transformWith: '`@rcv crTrace: `@arg' -> '`@rcv newLineTrace: `@arg'.

	self newLineTrace: aString
]

{ #category : '*Deprecated15-Kernel' }
Object >> traceCr [
	"Will be removed in Pharo 19."

	self
		deprecated: 'Use #traceNewLine instead.'
		on: '09/11/2026'
		in: #Pharo15
		transformWith: '`@receiver traceCr' -> '`@receiver traceNewLine'.
	self traceNewLine
]

{ #category : '*Deprecated15-Kernel' }
Object >> traceCr: anObject [
	"Will be removed in Pharo 19."

	self
		deprecated: 'Use #traceNewLine: instead.'
		on: '09/11/2026'
		in: #Pharo15
		transformWith: '`@receiver traceCr: `@argument' -> '`@receiver traceNewLine: `@argument'.
	self traceNewLine: anObject
]

{ #category : '*Deprecated15-Kernel' }
Object >> traceCrTab: anObject [

	self deprecated: 'Use #traceNewLineTab: instead.' transformWith: '`@rcv traceCrTab: `@arg' -> '`@rcv traceNewLineTab: `@arg'.

	self traceNewLineTab: anObject
]
