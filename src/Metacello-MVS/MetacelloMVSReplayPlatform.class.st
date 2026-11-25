"
I make repositories as written, but for those a recording saw moved (see MetacelloMVSRecordingPlatform): those I make at their recorded description, as the image's platform did, without network.
"
Class {
	#name : 'MetacelloMVSReplayPlatform',
	#superclass : 'MetacelloPlatform',
	#instVars : [
		'redirects'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSReplayPlatform class >> redirects: aDictionary [
	"Written repository description -> the description to make the repository at instead"

	^ self new setRedirects: aDictionary
]

{ #category : 'repositories' }
MetacelloMVSReplayPlatform >> createRepository: aRepositorySpec [
	"The repository as written, at its recorded new place when it was moved"

	^ super createRepository: (redirects
			   at: aRepositorySpec description
			   ifPresent: [ :moved |
					   aRepositorySpec copy
						   description: moved;
						   yourself ]
			   ifAbsent: [ aRepositorySpec ])
]

{ #category : 'repositories' }
MetacelloMVSReplayPlatform >> setRedirects: aDictionary [

	redirects := aDictionary
]
