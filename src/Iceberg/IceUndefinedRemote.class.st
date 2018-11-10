"
I am a null object representing a not found remote. I'm mainly used for polimorphism when the well known remote 'origin' is not found in the repository.
"
Class {
	#name : #IceUndefinedRemote,
	#superclass : #IceRemote,
	#category : #'Iceberg-Core'
}

{ #category : #testing }
IceUndefinedRemote class >> canHandleUrl: aRepositoryUrl [
	^ false
]

{ #category : #fetching }
IceUndefinedRemote >> fetch [
	
	IceRemoteNotFoundError new
		remoteName: name;
		signal: 'Please configure a pull remote for pulling.'
]

{ #category : #accessing }
IceUndefinedRemote >> host [
	^ nil
]

{ #category : #initialization }
IceUndefinedRemote >> initialize [
	super initialize.
	url := 'No remote url'.
	projectName := 'No name'.
	name := 'No remote'
]

{ #category : #testing }
IceUndefinedRemote >> isUndefined [
	^ true
]

{ #category : #accessing }
IceUndefinedRemote >> upstreamForBranch: aBranch backend: backend [
	^ nil
]
