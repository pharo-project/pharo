"
I am a version source that answers what another source answers, and records it as an MetacelloMVSRecordedSource (recorded), to keep as a fixture: tags, dates, failures to read tags, and the baseline definitions of each version read. Only what the resolution asks is recorded.
"
Class {
	#name : 'MetacelloMVSRecordingSource',
	#superclass : 'MetacelloMVSVersionSource',
	#instVars : [
		'source',
		'recorded'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'recording' }
MetacelloMVSRecordingSource >> baselineOf: aCandidate [

	| baseline |
	baseline := source baselineOf: aCandidate.
	recorded repository: aCandidate repository at: aCandidate version string baseline: baseline.
	^ baseline
]

{ #category : 'recording' }
MetacelloMVSRecordingSource >> recorded [
	"What the source answered so far, as an MetacelloMVSRecordedSource"

	^ recorded
]

{ #category : 'recording' }
MetacelloMVSRecordingSource >> source: aVersionSource [

	source := aVersionSource.
	recorded := MetacelloMVSRecordedSource new
]

{ #category : 'accessing' }
MetacelloMVSRecordingSource >> versionsOf: aRepository ifFail: aBlock [

	| answer |
	answer := source versionsOf: aRepository ifFail: [ :text |
			          recorded repository: aRepository failsWith: text.
			          ^ aBlock value: text ].
	recorded
		repository: aRepository versions: (answer collect: [ :each | each name ]);
		repository: aRepository
		dates: ((answer select: [ :each | each date notNil ]) collect: [ :each | each name -> each date ]) asDictionary.
	^ answer
]
