"
I am a null object representing the absence of a commit. I'm used generally in the case of unborn branches, or when a package was loaded but no iceberg information is available.
"
Class {
	#name : #IceNoCommit,
	#superclass : #IceCommitish,
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceNoCommit class >> repository: aRepository [

	^ self new
		repository: aRepository;
		yourself
]

{ #category : #comparing }
IceNoCommit >> = aNoCommit [

	^ aNoCommit isNoCommit
]

{ #category : #'private-changes' }
IceNoCommit >> changedPackagesToCommitInfo: anIceGitCommit [
	
	^ anIceGitCommit packages
]

{ #category : #'API-changes' }
IceNoCommit >> changesFromCommit: anIceNoCommit [ 
	
	^ anIceNoCommit changesFromNoCommit: self
]

{ #category : #'API-changes' }
IceNoCommit >> changesFromNoCommit: anIceNoCommit [ 
	
	^ #()
]

{ #category : #'API-changes' }
IceNoCommit >> changesTo: aCommitish [

	^ aCommitish changesFromNoCommit: self
]

{ #category : #accessing }
IceNoCommit >> commit [
	
	^ self
]

{ #category : #'API-changes' }
IceNoCommit >> commonAncestorWith: aCommitish [

	^ aCommitish
]

{ #category : #printing }
IceNoCommit >> description [
	
	^ 'No code loaded'
]

{ #category : #'file-access' }
IceNoCommit >> fileSystem [
	
	"This should belong to a Git specific No-Commit only..."
	^ FileSystem memory root
]

{ #category : #comparing }
IceNoCommit >> hash [

	^ self class hash
]

{ #category : #'API-packages' }
IceNoCommit >> includesPackageNamed: aName [
	
	^ false
]

{ #category : #testing }
IceNoCommit >> isAncestorOf: aCommit [

	^ false
]

{ #category : #testing }
IceNoCommit >> isLoaded [
	
	^ false
]

{ #category : #testing }
IceNoCommit >> isNoCommit [

	^ true
]

{ #category : #testing }
IceNoCommit >> isParentOf: aCommit [
	
	^ false
]

{ #category : #testing }
IceNoCommit >> isUnknownCommit [
	
	^ false
]

{ #category : #'private-monticello' }
IceNoCommit >> mcCommit [

	"This method is just for compatibility.
	I shold be used only in case of commit to an unborn branch"
	
	^ MCVersion new
]

{ #category : #'API-packages' }
IceNoCommit >> packageNames [
	
	^ #()
]

{ #category : #'API-project' }
IceNoCommit >> project [

	^ repository newUnbornProject
]

{ #category : #accessing }
IceNoCommit >> repository [

	^ repository
]

{ #category : #accessing }
IceNoCommit >> repository: aRepository [

	repository := aRepository
]

{ #category : #'private-monticello' }
IceNoCommit >> snapshotFor: anIceSavedPackage [ 
	
	^ MCSnapshot empty
]

{ #category : #'API-properties' }
IceNoCommit >> writerClass [

	^ self properties writerClass
]
