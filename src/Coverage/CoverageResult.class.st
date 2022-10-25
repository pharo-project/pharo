"
I represents the result of a code coverage measure.

My instances are produced by `CoverageCollector>>#collectResult`.
They contains the set of covered methods and sequence nodes.

The original CoverageCollector is known so that usfull information (like `CoverageResult>>#percentage`) can be computed.
"
Class {
	#name : #CoverageResult,
	#superclass : #Object,
	#instVars : [
		'collector',
		'methods',
		'nodes'
	],
	#category : #'Coverage-Base'
}

{ #category : #accessing }
CoverageResult >> collector [

	^ collector
]

{ #category : #accessing }
CoverageResult >> collector: anObject [

	collector := anObject
]

{ #category : #accessing }
CoverageResult >> methods [

	^ methods
]

{ #category : #accessing }
CoverageResult >> methods: anObject [

	methods := anObject
]

{ #category : #accessing }
CoverageResult >> nodes [

	^ nodes
]

{ #category : #accessing }
CoverageResult >> nodes: anObject [

	nodes := anObject
]

{ #category : #computation }
CoverageResult >> percentage [

	collector nodes isEmpty ifTrue: [ ^ Float nan ].
	^ 100.0 * nodes size / collector nodes size
]

{ #category : #printing }
CoverageResult >> printOn: aStream [

	"Append a sequence of characters to aStream that identify the receiver."

	super printOn: aStream.
	aStream
		nextPutAll: ' methods: ';
		print: methods size;
		nextPutAll: ' (';
		print: ((100.0 * methods size / collector methods size) rounded) ;
		nextPutAll: '%) nodes: ';
		print: nodes size;
		nextPutAll: ' (';
		print: ((100.0 * nodes size / collector nodes size) rounded) ;
		nextPutAll: '%)'

]

{ #category : #computation }
CoverageResult >> uncoveredMethods [

	^ collector methods \ methods
]

{ #category : #computation }
CoverageResult >> uncoveredNodes [

	^ collector nodes \ nodes
]
