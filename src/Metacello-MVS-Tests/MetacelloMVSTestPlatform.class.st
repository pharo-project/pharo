"
I am a Metacello platform for tests: I create the repositories I was given for their descriptions, and Monticello's otherwise.
"
Class {
	#name : 'MetacelloMVSTestPlatform',
	#superclass : 'MetacelloPlatform',
	#instVars : [
		'repositories'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'repositories' }
MetacelloMVSTestPlatform >> createRepository: aRepositorySpec [

	^ repositories at: aRepositorySpec description ifAbsent: [ super createRepository: aRepositorySpec ]
]

{ #category : 'repositories' }
MetacelloMVSTestPlatform >> initialize [

	super initialize.
	repositories := Dictionary new
]

{ #category : 'repositories' }
MetacelloMVSTestPlatform >> repository: aDescription is: aRepository [

	repositories at: aDescription put: aRepository
]
