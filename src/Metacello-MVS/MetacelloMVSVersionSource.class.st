"
I am where mvs learns about repositories: their versions (and the dates of their commits), and, for a version, the definitions of its baseline (an MetacelloMVSBaselineDefinitions). I answer data only: MetacelloMVSGraph runs baselines in the sandbox.

Subclasses: MetacelloMVSRecordedSource (versions and baselines given up front: replayed from a recording, or written by hand in tests), MetacelloMVSRecordingSource (records another source). The source that reads git makes itself the default when loaded (beDefault).
"
Class {
	#name : 'MetacelloMVSVersionSource',
	#superclass : 'Object',
	#classInstVars : [
		'defaultClass'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'default' }
MetacelloMVSVersionSource class >> beDefault [
	"Make my instances the source mvs reads repositories from"

	MetacelloMVSVersionSource defaultClass: self
]

{ #category : 'accessing' }
MetacelloMVSVersionSource class >> default [
	"A new source of the kind mvs reads repositories from: the one a source package made the default (beDefault)"

	^ self defaultClass new
]

{ #category : 'accessing' }
MetacelloMVSVersionSource class >> defaultClass [
	"Only meaningful on MetacelloMVSVersionSource itself"

	^ defaultClass ifNil: [ self error: 'mvs has no source to read repositories from: load one that calls beDefault' ]
]

{ #category : 'default' }
MetacelloMVSVersionSource class >> defaultClass: aSourceClass [

	defaultClass := aSourceClass
]

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
