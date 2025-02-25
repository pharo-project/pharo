"
I am a specialized analysis that limits the size of the serialized graph to a configured number of objects.

I know how many object have been analyzed and when the configured limit is reached. When the limit has been reached I can signal an exception (if configured) and will ignore all further objects.

I am used by FLSerialization to build the graph of objects to serialize.
"
Class {
	#name : 'FLObjectLimitingAnalysis',
	#superclass : 'FLAnalysis',
	#instVars : [
		'objectCounter',
		'objectLimit'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLObjectLimitingAnalysis class >> isResponsible [
	^ self configuration hasObjectsLimit
]

{ #category : 'instance creation' }
FLObjectLimitingAnalysis class >> priority [
	^ 30
]

{ #category : 'initialization' }
FLObjectLimitingAnalysis >> initialize [
	super initialize.
	
	objectCounter := 0.
	objectLimit := self configuration objectLimit
]

{ #category : 'mapping' }
FLObjectLimitingAnalysis >> mapAndTrace: anObject [
	"Don't count nil as an object"
	anObject ifNil: [
		super mapAndTrace: anObject.
		^ self ].
	
	objectCounter := objectCounter + 1.
	
	objectCounter > objectLimit
		ifTrue: [
			self context handleObjectLimitExceeded.
			firstMapper
				visitSubstitution: anObject
				by: nil ]
		ifFalse: [ super mapAndTrace: anObject ]
]
