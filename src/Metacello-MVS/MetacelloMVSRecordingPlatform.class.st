"
While recording, I stand in for the image's platform. I make repositories with it, and record in a MetacelloMVSRecordedSource each one it answers under another description than the one written: a repository moved on GitHub answers its new place (github://olekscode/SingularizePluralize is now pharo-contributions/SingularizePluralize). A MetacelloMVSReplayPlatform replays these redirects without network.
"
Class {
	#name : 'MetacelloMVSRecordingPlatform',
	#superclass : 'MetacelloPlatform',
	#instVars : [
		'platform',
		'recorded'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSRecordingPlatform class >> on: aPlatform recordingInto: aRecordedSource [
	"Make repositories with aPlatform, recording into aRecordedSource the ones that were moved"

	^ self new setPlatform: aPlatform recorded: aRecordedSource
]

{ #category : 'repositories' }
MetacelloMVSRecordingPlatform >> createRepository: aRepositorySpec [
	"The repository the image's platform makes, recorded when it lives at another place than the written one: it was moved. Places are compared, not descriptions: Iceberg describes the same repository in its own way (git@github.com:owner/name.git[v1.0.0])"

	| repository |
	repository := platform createRepository: aRepositorySpec.
	repository projectUrl = (super createRepository: aRepositorySpec) projectUrl ifFalse: [
		recorded redirect: aRepositorySpec description to: repository description ].
	^ repository
]

{ #category : 'repositories' }
MetacelloMVSRecordingPlatform >> setPlatform: aPlatform recorded: aRecordedSource [

	platform := aPlatform.
	recorded := aRecordedSource
]
