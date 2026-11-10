"
I am a version source told by hand, for tests: the tags and branches of each repository, with the baseline of each, given as the source of its baseline: method (or as MetacelloMVSBaselineDefinitions).
"
Class {
	#name : 'MetacelloMVSMemorySource',
	#superclass : 'MetacelloMVSVersionSource',
	#instVars : [
		'dates',
		'baselines'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'versions' }
MetacelloMVSMemorySource >> baselineOf: aCandidate [

	^ (baselines
		   at: aCandidate repository
		   ifAbsent: [ ^ MetacelloMVSBaselineDefinitions error: #repositoryFailure text: 'No repository ' , aCandidate repository ])
		  at: aCandidate version string
		  ifAbsent: [
				  MetacelloMVSBaselineDefinitions
					  error: #repositoryFailure
					  text: aCandidate repository , ' has no version ' , aCandidate version string printString ]
]

{ #category : 'versions' }
MetacelloMVSMemorySource >> datesOf: aRepository [

	^ dates at: aRepository ifAbsent: [ Dictionary new ]
]

{ #category : 'versions' }
MetacelloMVSMemorySource >> initialize [

	super initialize.
	baselines := Dictionary new.
	dates := Dictionary new
]

{ #category : 'building' }
MetacelloMVSMemorySource >> repository: aRepository ref: aVersion baseline: aBaselineDefinitions [

	(baselines at: aRepository ifAbsentPut: [ Dictionary new ]) at: aVersion put: aBaselineDefinitions
]

{ #category : 'building' }
MetacelloMVSMemorySource >> repository: aRepository version: aVersion date: aDateAndTime [

	(dates at: aRepository ifAbsentPut: [ Dictionary new ]) at: aVersion put: aDateAndTime
]

{ #category : 'building' }
MetacelloMVSMemorySource >> repository: aRepository version: aVersion project: aProjectName baseline: aMethodSource [
	"aVersion of aRepository, whose BaselineOf<aProjectName> has the baseline: method aMethodSource"

	self
		repository: aRepository
		ref: aVersion
		baseline:
		(MetacelloMVSBaselineDefinitions
			 baselineClassNamed: 'BaselineOf' , aProjectName
			 method: aMethodSource
			 repository: aRepository , ':' , aVersion)
]

{ #category : 'versions' }
MetacelloMVSMemorySource >> versionsOf: aRepository ifFail: aBlock [
	"The versions it has a baseline for"

	^ (baselines at: aRepository ifAbsent: [ ^ aBlock value: 'No repository ' , aRepository ]) keys asArray
]
