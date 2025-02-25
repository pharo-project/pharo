"
I am a specialized analysis that limits the depth of the serialized graph to a configured number.

I know how the degree of the node being analyzed and the configured limit. Whenever the degree of the analyzed node exceeds the limit I will ignore the current node and continue with the next node.

I am used by FLSerialization to build the graph of objects to serialize.
"
Class {
	#name : 'FLDepthLimitingAnalysis',
	#superclass : 'FLAnalysis',
	#instVars : [
		'depthLimit',
		'depth',
		'previousStackSize'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLDepthLimitingAnalysis class >> isResponsible [
	^ self configuration hasDepthLimit
]

{ #category : 'instance creation' }
FLDepthLimitingAnalysis class >> priority [
	^ 20
]

{ #category : 'initialization' }
FLDepthLimitingAnalysis >> initialize [
	super initialize.
	
	depthLimit := self configuration depthLimit.
	previousStackSize := 0.
	depth := 0
]

{ #category : 'mapping' }
FLDepthLimitingAnalysis >> mapAndTrace: anObject [
	objectStack size - previousStackSize >= 0
		ifTrue: [ depth := depth + 1 ].
	previousStackSize := objectStack size.
	
	(anObject isNotNil and: [
		depth  > depthLimit])
			ifTrue: [
				firstMapper
					visitSubstitution: anObject
					by: nil ]
			ifFalse: [ super mapAndTrace: anObject ]
]
