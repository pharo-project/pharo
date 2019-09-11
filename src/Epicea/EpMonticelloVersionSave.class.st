"
I represent a Monticello ""Save Version"" operation performed in the system.
"
Class {
	#name : #EpMonticelloVersionSave,
	#superclass : #EpEvent,
	#instVars : [
		'versionName',
		'repositoryDescription'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpMonticelloVersionSave class >> version: aMCVersion repository: aMCRepository [ 
	^ self 
		versionName: aMCVersion info name 
		repositoryDescription: aMCRepository description
]

{ #category : #'instance creation' }
EpMonticelloVersionSave class >> versionName: aVersionName repositoryDescription: aRepositoryDescription [ 
	^ self basicNew
		initializeWithVersionName: aVersionName
		repositoryDescription: aRepositoryDescription;
		yourself
]

{ #category : #comparing }
EpMonticelloVersionSave >> = another [

	^ self class = another class
		and: [ self versionName = another versionName
		and: [ self repositoryDescription = another repositoryDescription ] ]
]

{ #category : #visitor }
EpMonticelloVersionSave >> accept: aChangeVisitor [ 
	^ aChangeVisitor visitMonticelloVersionSave: self
]

{ #category : #accessing }
EpMonticelloVersionSave >> affectedPackageName [
	
	^ self packageName
]

{ #category : #comparing }
EpMonticelloVersionSave >> hash [

	^ (self class hash
		bitXor: self versionName hash)
		bitXor: self repositoryDescription hash
]

{ #category : #initialization }
EpMonticelloVersionSave >> initializeWithVersionName: aVersionName repositoryDescription: aRepositoryDescription [ 
	self initialize.
	
	versionName := aVersionName.
	repositoryDescription := aRepositoryDescription.
]

{ #category : #testing }
EpMonticelloVersionSave >> isMonticelloVersionSave [

	^ true
]

{ #category : #accessing }
EpMonticelloVersionSave >> packageName [

	^ versionName copyUpToLast: $-
]

{ #category : #printing }
EpMonticelloVersionSave >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: repositoryDescription;
		nextPut: $/;
		nextPutAll: versionName;
		nextPut: $).
]

{ #category : #accessing }
EpMonticelloVersionSave >> repositoryDescription [
	^ repositoryDescription
]

{ #category : #accessing }
EpMonticelloVersionSave >> versionName [
	^ versionName
]
