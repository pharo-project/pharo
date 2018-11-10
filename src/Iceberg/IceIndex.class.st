"
I represent an index of temporary stored files for later commit. I'm used internally, usually people should not use me.

My main responsibility is to store temporary changes and commit them.

However, I'm an abstract class, check my subclasses for more concrete information.
"
Class {
	#name : #IceIndex,
	#superclass : #Object,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceIndex class >> on: anIceLibgitRepository [ 
	
	^ self new
		repository: anIceLibgitRepository;
		yourself
]

{ #category : #adding }
IceIndex >> addPackage: aPackage [
	
	self subclassResponsibility
]

{ #category : #'API-commiting' }
IceIndex >> commitWithMessage: message andParents: parentCommits [

	self subclassResponsibility.	

]

{ #category : #testing }
IceIndex >> isEmpty [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceIndex >> repository [
	^ repository
]

{ #category : #accessing }
IceIndex >> repository: anIceLibgitRepository [ 
	repository := anIceLibgitRepository
]

{ #category : #storing }
IceIndex >> storeVersion: aMCVersion [ 
	
	self subclassResponsibility
]

{ #category : #updating }
IceIndex >> updateDiskWorkingCopy: anIceDiff [ 
	
	self subclassResponsibility
]

{ #category : #updating }
IceIndex >> updateIndex: anIceDiff [ 

	anIceDiff tree
		accept: (IceIndexUpdateVisitor new
			index: self;
			diff: anIceDiff).
]
