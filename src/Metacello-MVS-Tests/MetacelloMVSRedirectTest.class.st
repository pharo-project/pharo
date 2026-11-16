"
I test how a recording keeps moved repositories: MetacelloMVSRecordingPlatform records them, MetacelloMVSReplayPlatform makes them at their new place.
"
Class {
	#name : 'MetacelloMVSRedirectTest',
	#superclass : 'TestCase',
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'running' }
MetacelloMVSRedirectTest >> moved [
	"The redirect GitHub answers for SingularizePluralize"

	^ { ('github://olekscode/SingularizePluralize:v1.0/src' -> 'github://pharo-contributions/SingularizePluralize:v1.0/src') }
		  asDictionary
]

{ #category : 'running' }
MetacelloMVSRedirectTest >> repositoryOf: aString on: aPlatform [

	^ aPlatform createRepository: (MetacelloMVSResolverTest specsAt: { aString }) first repositorySpecs first
]

{ #category : 'tests' }
MetacelloMVSRedirectTest >> testARecordingKeepsItsRedirectsWhenStored [

	| recorded |
	recorded := MetacelloMVSRecordedSource new.
	recorded
		redirect: 'github://olekscode/SingularizePluralize:v1.0/src'
		to: 'github://pharo-contributions/SingularizePluralize:v1.0/src'.
	self assert: (self class compiler evaluate: recorded storeString) redirects equals: self moved
]

{ #category : 'tests' }
MetacelloMVSRedirectTest >> testTheRecordingKeepsOnlyMovedRepositories [
	"The wrapped platform moves olekscode's repository, as GitHub's redirect does"

	| recorded platform |
	recorded := MetacelloMVSRecordedSource new.
	platform := MetacelloMVSRecordingPlatform on: (MetacelloMVSReplayPlatform redirects: self moved) recordingInto: recorded.
	self
		assert: (self repositoryOf: 'github://olekscode/SingularizePluralize:v1.0/src' on: platform) projectPath
		equals: 'pharo-contributions/SingularizePluralize'.
	self repositoryOf: 'github://acme/Foo:v1.0.0' on: platform.
	self assert: recorded redirects equals: self moved
]

{ #category : 'tests' }
MetacelloMVSRedirectTest >> testTheReplayMakesAMovedRepositoryAtItsNewPlace [

	| repository |
	repository := self
		              repositoryOf: 'github://olekscode/SingularizePluralize:v1.0/src'
		              on: (MetacelloMVSReplayPlatform redirects: self moved).
	self
		assert: repository projectPath equals: 'pharo-contributions/SingularizePluralize';
		assert: repository projectVersion equals: 'v1.0'
]

{ #category : 'tests' }
MetacelloMVSRedirectTest >> testTheReplayMakesOtherRepositoriesAsWritten [

	| repository |
	repository := self repositoryOf: 'github://acme/Foo:v1.0.0' on: (MetacelloMVSReplayPlatform redirects: self moved).
	self assert: repository projectPath equals: 'acme/Foo'
]
