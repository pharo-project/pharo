"
I am where mvs learns about repositories: their versions (each with a name and a date, like a git tag), and, for a version, the definitions of its baseline (an MetacelloMVSBaselineDefinitions). I answer data only: MetacelloMVSGraph runs baselines in the sandbox.

Subclasses: MetacelloMVSRepositorySource (asks the repositories Metacello creates; what mvs uses), MetacelloMVSRecordedSource (versions and baselines given up front: replayed from a recording, or written by hand in tests), MetacelloMVSRecordingSource (records another source).
"
Class {
	#name : 'MetacelloMVSVersionSource',
	#superclass : 'Object',
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'versions' }
MetacelloMVSVersionSource >> baselineOf: aCandidate [
	"The MetacelloMVSBaselineDefinitions of the baseline of the version of aCandidate. Never signals: what cannot be read becomes a problem of the answer"

	^ self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloMVSVersionSource >> versionsOf: aRepository ifFail: aBlock [
	"The versions aRepository publishes, each answering name and date (a date may be nil), like git tags; when they cannot be read, the value of aBlock with why"

	^ self subclassResponsibility
]
