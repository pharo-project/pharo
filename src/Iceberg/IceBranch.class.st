"
I am an abstract class representing a branch. My history and accessing operations delegate in my current commit. The only exception is checkout operation that checkouts and sets myself as HEAD.

Check my superclass and subclasses for more information.
"
Class {
	#name : #IceBranch,
	#superclass : #IceCommitish,
	#instVars : [
		'name'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceBranch class >> named: aName inRepository: aRepository [

	| branch |
	branch := super inRepository: aRepository.
	branch name: aName.
	^ branch
]

{ #category : #'API-changes' }
IceBranch >> changesFromCommit: anIceCommit [
	
	^ self commit changesFromCommit: anIceCommit
]

{ #category : #'API-history' }
IceBranch >> commitsDo: aBlockClosure [ 
	
	self newCommitWalk commitsDo: aBlockClosure
]

{ #category : #'API-branches' }
IceBranch >> delete [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceBranch >> description [
	
	^ self name
]

{ #category : #accessing }
IceBranch >> fullname [

	^ name
]

{ #category : #'API-history' }
IceBranch >> includesCommit: aCommit [
	
	^ self newCommitWalk includesCommit: aCommit
]

{ #category : #'API-packages' }
IceBranch >> includesPackageNamed: aString [
	
	^ self commit includesPackageNamed: aString
]

{ #category : #testing }
IceBranch >> isBranch [
	
	^ true
]

{ #category : #testing }
IceBranch >> isHead [
	^ self repository branch = self
]

{ #category : #testing }
IceBranch >> isLocal [
	
	^ self isRemote not
]

{ #category : #accessing }
IceBranch >> name [
	^ name
]

{ #category : #accessing }
IceBranch >> name: aString [ 
	name := aString
]

{ #category : #'API-history' }
IceBranch >> newCommitWalk [

	^ repository newCommitWalk fromBranch: self
]

{ #category : #'API-packages' }
IceBranch >> packageNames [
	
	^ self commit packageNames
]

{ #category : #'API-packages' }
IceBranch >> packages [
	
	^ self commit packages
]

{ #category : #printing }
IceBranch >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ')'
]

{ #category : #'API-properties' }
IceBranch >> properties [
	
	^ self commit properties
]

{ #category : #'private-monticello' }
IceBranch >> snapshotFor: anIceSavedPackage [ 
	
	^ self commit snapshotFor: anIceSavedPackage
]

{ #category : #validation }
IceBranch >> validateIsBranch [
	
	"Nothing. I'm a branch"
]

{ #category : #'private-changes' }
IceBranch >> versionFor: anIcePackage [ 
	
	^ self commit versionFor: anIcePackage
]
