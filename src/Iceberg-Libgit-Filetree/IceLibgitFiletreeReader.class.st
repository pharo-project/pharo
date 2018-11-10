"
Reads an MCVersion from a git repository (its blob, not a working copy!) using libgit bindings.

Currently we only support metadata-less repositories.
"
Class {
	#name : #IceLibgitFiletreeReader,
	#superclass : #MCFileTreeStCypressReader,
	#traits : 'TIceRepositoryReader',
	#classTraits : 'TIceRepositoryReader classTrait',
	#instVars : [
		'iceVersion'
	],
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #accessing }
IceLibgitFiletreeReader class >> compatibleAuthorNameOf: aString [
	"Timestamp of method has a very precise format that needs to be fulfiled: 
		
		'FirstnameLastname MM/DD/YYYY HH/MM' 
	
	Problem is that git username is different: Firstname Lastname.
	To make this compatible, we remove spaces... but this can be a problem in the future... 
	we'll see."
	^ aString reject: [ :each | each = Character space ]
]

{ #category : #testing }
IceLibgitFiletreeReader class >> id [
	^ #filetree
]

{ #category : #testing }
IceLibgitFiletreeReader class >> isLegacyDefault [
	^ true
]

{ #category : #'instance creation' }
IceLibgitFiletreeReader class >> on: aCommitOrTree iceVersion: iceVersion [
	^ (self on: aCommitOrTree) 
		iceVersion: iceVersion; 
		yourself
]

{ #category : #accessing }
IceLibgitFiletreeReader >> commit [
	^ stream
]

{ #category : #accessing }
IceLibgitFiletreeReader >> fileUtils [
	^ IceLibgitFiletreeFileUtils
]

{ #category : #testing }
IceLibgitFiletreeReader >> hasMonticelloMetadata [
	^ false
]

{ #category : #accessing }
IceLibgitFiletreeReader >> iceVersion [
	^ iceVersion
]

{ #category : #accessing }
IceLibgitFiletreeReader >> iceVersion: anObject [
	iceVersion := anObject.
	self packageDirectory: (iceVersion commit project sourceDirectoryPath / (IcePackage named: iceVersion package name repository: iceVersion commit repository) directoryName) pathString
]

{ #category : #accessing }
IceLibgitFiletreeReader >> loadVersionInfo [
	^ info := self iceVersion info
]

{ #category : #accessing }
IceLibgitFiletreeReader >> packageNameFromPackageDirectory [
	^ iceVersion packageName
]

{ #category : #accessing }
IceLibgitFiletreeReader >> versionName [
	^ ('{1}-{2}.{3}' format: {
		self packageNameFromPackageDirectory. 
		self commit compatibleUsername. 
		self versionNumber
	})
]
