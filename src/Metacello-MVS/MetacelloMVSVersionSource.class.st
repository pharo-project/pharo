"
I am where mvs learns about repositories: their tags (and the dates they point to), their default branch, and, for a version, the definitions of its baseline (an MetacelloMVSBaselineDefinitions). I answer data only: MetacelloMVSGraph runs baselines in the sandbox.

Subclasses: MetacelloMVSMemorySource (told, for tests), MetacelloMVSRecordedSource (replays a recording), MetacelloMVSRecordingSource (records another source). Reading git lives outside Metacello (MetacelloMVSIcebergSource, which makes itself the default when loaded).
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

{ #category : 'default' }
MetacelloMVSVersionSource class >> default [
	"A new source of the kind mvs reads repositories from: the one a source package made the default (MetacelloMVSIcebergSource when Iceberg is loaded), or else an MetacelloMVSMemorySource, which knows no repository"

	^ MetacelloMVSVersionSource defaultClass new
]

{ #category : 'default' }
MetacelloMVSVersionSource class >> defaultClass [
	"Only meaningful on MetacelloMVSVersionSource itself"

	^ defaultClass ifNil: [ MetacelloMVSMemorySource ]
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

{ #category : 'versions' }
MetacelloMVSVersionSource >> datesOf: aRepository [
	"Version -> DateAndTime of what each version points to; empty when not known"

	^ Dictionary new
]

{ #category : 'versions' }
MetacelloMVSVersionSource >> versionsOf: aRepository ifFail: aBlock [
	"The versions of aRepository; when it cannot be read, the value of aBlock with why"

	^ self subclassResponsibility
]
