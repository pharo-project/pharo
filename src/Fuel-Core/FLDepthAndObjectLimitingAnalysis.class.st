"
I am a specialized analysis that limits the size of the serialized graph to a configured number of objects and also limits the depth of the graph to a configured degree.

See the comments of FLDepthLimitingAnalysis and FLObjectLimitingAnalysis for more information.
"
Class {
	#name : 'FLDepthAndObjectLimitingAnalysis',
	#superclass : 'FLAnalysis',
	#instVars : [
		'depthLimit',
		'depth',
		'objectCounter',
		'objectLimit',
		'previousStackSize'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLDepthAndObjectLimitingAnalysis class >> isResponsible [
	^ self configuration hasDepthLimit and: [
		self configuration hasObjectsLimit ]
]

{ #category : 'instance creation' }
FLDepthAndObjectLimitingAnalysis class >> priority [
	^ 5
]

{ #category : 'initialization' }
FLDepthAndObjectLimitingAnalysis >> initialize [
	super initialize.
	
	objectCounter := 0.
	objectLimit := self configuration objectLimit.
	previousStackSize := 0.
	depth := 0.
	depthLimit := self configuration depthLimit
]

{ #category : 'mapping' }
FLDepthAndObjectLimitingAnalysis >> mapAndTrace: anObject [
	anObject ifNil: [
		super mapAndTrace: anObject.
		^ self ].
	
	objectStack size - previousStackSize >= 0 ifTrue: [
		depth := depth + 1 ].
	previousStackSize := objectStack size.
	
	objectCounter := objectCounter + 1.
	
	objectCounter > objectLimit ifTrue: [
		self context handleObjectLimitExceeded ].
	
	(objectCounter > objectLimit or: [
		depth > depthLimit ])
			ifTrue: [
				firstMapper
					visitSubstitution: anObject
					by: nil ]
			ifFalse: [ super mapAndTrace: anObject ]
]
