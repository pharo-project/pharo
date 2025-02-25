"
I am responsible of traversing the graph of references starting from a root object. I will produce a clusterization which the serializer will store on a stream.

An example of use is:

	(FLAnalyzer newDefault 
		clusterizationOf: (Array with: 1@2 with: 3@4))
		clusters.

"
Class {
	#name : 'FLAnalysis',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'objectStack',
		'firstMapper',
		'clusterization',
		'privateObjectStack',
		'privateFirstMapper'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLAnalysis class >> detectResponsibleAnalysis [
	^ (self allSubclasses sort: [ :a :b | a priority <= b priority ])
		detect: [ :class | class isResponsible ]
		ifNone: [ self ]
]

{ #category : 'instance creation' }
FLAnalysis class >> isResponsible [
	^ self subclassResponsibility
]

{ #category : 'instance creation' }
FLAnalysis class >> priority [
	^ self subclassResponsibility
]

{ #category : 'instance creation' }
FLAnalysis class >> run [
	^ self detectResponsibleAnalysis new
		run;
		yourself
]

{ #category : 'accessing' }
FLAnalysis >> clusterization [

	^clusterization
]

{ #category : 'initialization' }
FLAnalysis >> initialize [
	super initialize.
	
	objectStack := FLSimpleStack new.
	privateObjectStack := FLSimpleStack new.
	clusterization := FLClusterization new.
	
	self context analysis: self.

	self initializeMapperChains
]

{ #category : 'initialization' }
FLAnalysis >> initializeMapperChains [
	| standardMappers allMappers |
	standardMappers := self standardMappers.
	allMappers := self substitutionMappers asOrderedCollection
		addAll: standardMappers;
		reduceRight: [ :current :next | current next: next ];
		yourself.
	
	firstMapper := allMappers first.
	privateFirstMapper := standardMappers first
]

{ #category : 'mapping' }
FLAnalysis >> mapAndTrace: anObject [
	"Map an object to its cluster. Trace its references."

	firstMapper mapAndTrace: anObject
]

{ #category : 'mapping' }
FLAnalysis >> privateMapAndTrace: anObject [
	"Map an object to its cluster. Trace its references."

	privateFirstMapper mapAndTrace: anObject
]

{ #category : 'tracing' }
FLAnalysis >> privateTrace: anObject [

	privateObjectStack push: anObject
]

{ #category : 'running' }
FLAnalysis >> run [

	objectStack push: self context object.
	[ objectStack isEmpty ] 
		whileFalse: [ self mapAndTrace: objectStack pop ].
	objectStack := nil.	
	
	[ privateObjectStack isEmpty ] 
		whileFalse: [ self privateMapAndTrace: privateObjectStack pop ].
]

{ #category : 'accessing' }
FLAnalysis >> standardMappers [
	^ {
		FLLightGlobalMapper new.
		FLFullGeneralMapper new.
		FLLightGeneralMapper new
	}
]

{ #category : 'accessing' }
FLAnalysis >> substitutionMappers [
	^ self configuration substitutions collect: [ :assoc |
		FLPluggableSubstitutionMapper 
			when: assoc key
			substituteBy: assoc value ]
]

{ #category : 'tracing' }
FLAnalysis >> trace: anObject [
	"It's possible to get here from #privateMapAndTrace: but then #mapAndTrace: will
	not be sent again (see #run)"
	objectStack
		ifNil: [ self privateTrace: anObject ]
		ifNotNil: [ objectStack push: anObject ]
]
