"
I am responsible of traversing the graph of references starting from a root object. I will produce a clusterization which the serializer will store on a stream.

An example of use is:

	(FLAnalyzer newDefault 
		clusterizationOf: (Array with: 1@2 with: 3@4))
		clusters.

"
Class {
	#name : #FLAnalysis,
	#superclass : #Object,
	#instVars : [
		'objectStack',
		'firstMapper',
		'clusterization',
		'root',
		'privateObjectStack',
		'privateFirstMapper'
	],
	#category : 'Fuel-Core-Base'
}

{ #category : #'instance creation' }
FLAnalysis class >> newWith: aMapper private: privateMapper root: anObject [

	^self basicNew
		initializeWith: aMapper private: privateMapper root: anObject;
		yourself.
]

{ #category : #accessing }
FLAnalysis >> clusterization [

	^clusterization
]

{ #category : #hooks }
FLAnalysis >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #'initialize-release' }
FLAnalysis >> initializeWith: aMapper private: privateMapper root: anObject [

	self initialize.
	firstMapper := aMapper.
	privateFirstMapper := privateMapper.
	aMapper analysis: self.
	privateMapper analysis: self.
	root := anObject.
	objectStack := FLSimpleStack new.
	privateObjectStack := FLSimpleStack new.
	clusterization := FLClusterization new.

]

{ #category : #mapping }
FLAnalysis >> mapAndTrace: anObject [
	"Map an object to its cluster. Trace its references."

	firstMapper mapAndTrace: anObject fuelReplacement
]

{ #category : #mapping }
FLAnalysis >> privateMapAndTrace: anObject [
	"Map an object to its cluster. Trace its references."

	privateFirstMapper mapAndTrace: anObject
]

{ #category : #tracing }
FLAnalysis >> privateTrace: anObject [

	privateObjectStack push: anObject
]

{ #category : #analyzing }
FLAnalysis >> run [

	objectStack push: root.
	[ objectStack isEmpty ] 
		whileFalse: [ self mapAndTrace: objectStack pop ].
	[ privateObjectStack isEmpty ] 
		whileFalse: [ self privateMapAndTrace: privateObjectStack pop ].
]

{ #category : #tracing }
FLAnalysis >> trace: anObject [

	objectStack push: anObject
]
