"
I am an abstract superclass useful for visitors of EpLogBrowserOperation.
"
Class {
	#name : 'EpLogBrowserOperationVisitor',
	#superclass : 'Object',
	#instVars : [
		'environment',
		'application'
	],
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'instance creation' }
EpLogBrowserOperationVisitor class >> inEnvironment: anEnvironment [

	^ self basicNew
		initializeWith: anEnvironment;
		yourself
]

{ #category : 'instance creation' }
EpLogBrowserOperationVisitor class >> new [
	^ self inEnvironment: self environment
]

{ #category : 'accessing' }
EpLogBrowserOperationVisitor >> application: anApplication [ 
	application := anApplication
]

{ #category : 'private' }
EpLogBrowserOperationVisitor >> behaviorNamed: aSymbol ifPresent: presentBlock [

	^ (environment classOrTraitNamed: aSymbol)
		ifNil: [ application inform: 'Cannot find ', aSymbol ]
		ifNotNil: presentBlock
]

{ #category : 'private' }
EpLogBrowserOperationVisitor >> compiler [

	^ self class compiler
]

{ #category : 'initialize' }
EpLogBrowserOperationVisitor >> initializeWith: anEnvironment [

	self initialize.
	environment := anEnvironment
]
