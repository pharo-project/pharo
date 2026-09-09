Extension { #name : 'Context' }

{ #category : '*Deprecated14' }
Context >> debugStackOn: aStream [
	"print the top contexts on my sender chain."

	self deprecated: 'Use printStackTraceOn: instead' transformWith: '`@rcv printDebugStackOn: `@arg' -> '`@rcv printStackTraceOn: `@arg'.
	
	self printStackTraceOn: aStream
]

{ #category : '*Deprecated14' }
Context >> longStack [
	"Answer a String showing the top 100 contexts on my sender chain."

	self deprecated: 'Use stackTraceString instead'
		transformWith: '`@rcvr longStack' -> '`@rcvr stackTraceString'.
	^ self stackTraceString
]

{ #category : '*Deprecated14' }
Context >> printDebugStackOn: aStream [

	"print the top 30 contexts on my sender chain."
	<reflection: 'Stack Manipulation - Context'>
	
	self deprecated: 'Use printStackTraceOn: instead' transformWith: '`@rcv printDebugStackOn: `@arg' -> '`@rcv printStackTraceOn: `@arg'.
	
	self printStackTraceOn: aStream
]

{ #category : '*Deprecated14' }
Context >> shortDebugStack [
	"Answer a String showing the top ten contexts on my sender chain."

	self
		deprecated: 'Use #stackTraceString instead'
		transformWith: '`@rcv shortDebugStack' -> '`@rcv stackTraceString'.

	^ self stackTraceString
]

{ #category : '*Deprecated14' }
Context >> shortDebugStackOn: aStream [

	"print the top 30 contexts on my sender chain."
	<reflection: 'Stack Manipulation - Context'>
	
	self deprecated: 'Use printStackTraceOn: instead' transformWith: '`@rcv shortDebugStackOn: `@arg' -> '`@rcv printStackTraceOn: `@arg'.
	
	self printStackTraceOn: aStream
]

{ #category : '*Deprecated14' }
Context >> shortStack [
	"Answer a String showing the top ten contexts on my sender chain."

	self deprecated: 'Use stackTraceString instead'
		transformWith: '`@rcvr shortStack' -> '`@rcvr stackTraceString'.
	^ self stackTraceString
]
