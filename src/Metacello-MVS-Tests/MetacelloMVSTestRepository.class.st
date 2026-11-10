"
I am a repository for tests that knows its versions and the baseline at each, as Iceberg's repositories do, without git.
"
Class {
	#name : 'MetacelloMVSTestRepository',
	#superclass : 'MCRepository',
	#instVars : [
		'versions',
		'baselines'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'accessing' }
MetacelloMVSTestRepository >> baselineOf: aProjectName atVersion: aVersionName [

	^ baselines at: aVersionName
]

{ #category : 'accessing' }
MetacelloMVSTestRepository >> description [

	^ 'test repository'
]

{ #category : 'accessing' }
MetacelloMVSTestRepository >> initialize [

	super initialize.
	versions := OrderedCollection new.
	baselines := Dictionary new
]

{ #category : 'accessing' }
MetacelloMVSTestRepository >> version: aVersionName baseline: aCollectionOfDefinitions [
	"aVersionName has the baseline aCollectionOfDefinitions, or none when nil"

	versions add: (MetacelloProjectVersion name: aVersionName).
	baselines at: aVersionName put: aCollectionOfDefinitions
]

{ #category : 'accessing' }
MetacelloMVSTestRepository >> versions [

	^ versions
]
