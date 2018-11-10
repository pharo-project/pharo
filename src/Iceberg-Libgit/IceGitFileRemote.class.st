"
I am a 'remote' repository which is stored locally in your machine and accessed through a 'file://' url.
"
Class {
	#name : #IceGitFileRemote,
	#superclass : #IceGitRemote,
	#instVars : [
		'path'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #testing }
IceGitFileRemote class >> canHandleUrl: aRepositoryUrl [
	^ (aRepositoryUrl beginsWith: 'file:') or: [aRepositoryUrl beginsWith: '/']
]

{ #category : #accessing }
IceGitFileRemote >> detailedInfo [
	^ { 'Origin' -> path pathString }
]

{ #category : #accessing }
IceGitFileRemote >> host [
	^ 'local filesystem'
]

{ #category : #querying }
IceGitFileRemote >> organizer [
	^ 'local'
]

{ #category : #private }
IceGitFileRemote >> parseUrl [
	| znUrl |
	znUrl := ZnUrl fromString: url defaultScheme: 'file'.
	path := znUrl asFileReference.
	projectName := path basename.

]

{ #category : #accessing }
IceGitFileRemote >> projectName [
	^ path basename

]
