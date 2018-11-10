"
I represent a repository commit. As such, I hold information about commit message, date, author and id:

  aCommit id.
  aCommit author.
  aCommit datetime.
  aCommit comment.

I also provide operations to access my history:

  aCommit parent.
  aCommit ancestors.
  aCommit isAncestorOf: anotherCommit.
  aCommit isParentOf: anotherCommit.

And to the packages existing in that commit:

  aCommit packageNames.

IMPORTANT IMPLEMENTATION NOTE. For optimization purposes, a commit caches the names of the packages inside himself. This was made under the assumption that a commit is immutble and nobody ""cooks"" the commit once it is in the repository.
"
Class {
	#name : #IceCommit,
	#superclass : #IceCommitish,
	#instVars : [
		'packageCache'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'API-working copy' }
IceCommit >> adopt [
	
	"Low level.
	For the working copy to adopt myself as reference commit"
	
	self repository workingCopy adoptCommit: self
]

{ #category : #'API-history' }
IceCommit >> ancestors [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
IceCommit >> author [
	
	^ self subclassResponsibility
]

{ #category : #'API-changes' }
IceCommit >> canRevertChanges [
	^ false
]

{ #category : #'private-changes' }
IceCommit >> changedPackagesToWorkingCopy: left [ 
	
	| leftPackageNames rightPackageNames added removed |
	
	"We do not need to do a double dispatch to calculate a diff.
	diffBetween: a and: b
	  a snapshot difference: b snapshot.
	
	 - Monticello already knows how to do it
	 - the algorithm is in only one place
	 - exploit polymorphism
	"
	self flag: #refactor.
	
	leftPackageNames := left packageNames.
	rightPackageNames := self packageNames.
	added := (leftPackageNames \ rightPackageNames) 
		collect: [ :each | IcePackage named: each repository: self repository ].
	"this will give me the removals, but I'm still don't know what to do with them
	 (because if I just add it it will show them as added not as removed)"
	removed := (rightPackageNames \ leftPackageNames)
		reject: [ :each | self packageNames includes: each ]
		thenCollect: [ :each | IcePackage named: each repository: self repository ].
	
	^ added, ((left modifiedPackages
		reject: [ :p | (leftPackageNames \ rightPackageNames ) includes: p name ])
		collect: [ :mcPackage | IcePackage named: mcPackage name repository: repository ] as: Array),
			removed
]

{ #category : #'private-changes' }
IceCommit >> changesFromNoCommit: anIceNoCommit [ 

	^ self packages collect: [ :each | IceImageChange package: each ]
]

{ #category : #'API-changes' }
IceCommit >> changesTo: aCommitish [

	^ aCommitish changesFromCommit: self
]

{ #category : #accessing }
IceCommit >> comment [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceCommit >> commit [
	
	^ self
]

{ #category : #'API-history' }
IceCommit >> commonAncestorWith: aCommitish [
	
	^ aCommitish commonAncestorWithCommit: self
]

{ #category : #'private-monticello' }
IceCommit >> compatibleUsername [
	
	^ 'CompatibleUserName'
]

{ #category : #'API-tags' }
IceCommit >> createTag: aString [

	self subclassResponsibility
]

{ #category : #accessing }
IceCommit >> datetime [
	
	self subclassResponsibility
]

{ #category : #printing }
IceCommit >> description [
	
	self tags
		ifNotEmpty: [ :tags |
			^ ', ' join: (tags collect: [ :each | each name ]) ].
		
	^ 'Detached at {1}' format: { self shortId }
]

{ #category : #'API-changes' }
IceCommit >> diffFromHead [
	^ self repository headCommit diffTo: self
]

{ #category : #'API-changes' }
IceCommit >> diffToParent [
	"returns a diff between me and my first parent/ancestor"
	^ self diffTo: (self ancestors
			ifNotEmpty: #first
			ifEmpty: [ nil ])
]

{ #category : #'API-changes' }
IceCommit >> diffToWorkingCopy [
	^ self diffTo: self repository workingCopy
]

{ #category : #'API-changes' }
IceCommit >> diffToWorkingCopyForPackage: aPackage [

	^ self diffToWorkingCopyForPackages: { aPackage }
]

{ #category : #'API-changes' }
IceCommit >> diffToWorkingCopyForPackages: aCollection [

	^ IceDiff new
		sourceVersion: self;
		targetVersion: self repository workingCopy;
		buildForPackages: aCollection
]

{ #category : #testing }
IceCommit >> hasMultipleAncestors [
	| ancestors |
	ancestors := self ancestors.
	^ ancestors notNil 
		and: [ ancestors size > 1 ]
]

{ #category : #accessing }
IceCommit >> id [
	self subclassResponsibility
]

{ #category : #'API-packages' }
IceCommit >> includesPackageNamed: aName [
	
	^ self packageNames includes: aName
]

{ #category : #'API-history' }
IceCommit >> isAncestorOf: aCommit [
	
	^ aCommit isParentOf: self
]

{ #category : #testing }
IceCommit >> isMergeCommit [
	
	^ self ancestors size > 1
]

{ #category : #testing }
IceCommit >> isMerged [
	
	^ self repository workingCopy isCommitMerged: self
]

{ #category : #testing }
IceCommit >> isNoCommit [
	
	^ false
]

{ #category : #'API-history' }
IceCommit >> isParentOf: aCommit [

	^ self newCommitWalk includesCommit: aCommit
]

{ #category : #testing }
IceCommit >> isUnknownCommit [
	
	^ false
]

{ #category : #'private-monticello' }
IceCommit >> mcPackageSnapshotsOfPackages: aCollection [
	self flag: #todo. "I'm not sure I need to answer an empty snapshot. 
	if there is no mcVersion it likely means the package was removed and an empty snapshow will 
	keep it there."
	^ aCollection
		select: #isLoaded
		thenCollect: [ :package |
			[ (self repository mcVersionFor: 
				(IceSavedPackageVersion fromCommit: self package: package mcPackage))
					snapshot ]
			on: IceVersionDoesNotExist 
			do: [ :e | MCSnapshot empty ] ]
]

{ #category : #accessing }
IceCommit >> name [

	^ self id
]

{ #category : #'API-history' }
IceCommit >> newCommitWalk [

	^ repository newCommitWalk fromCommit: self
]

{ #category : #'API-packages' }
IceCommit >> packageNames [
	
	^ self subclassResponsibility
]

{ #category : #'API-packages' }
IceCommit >> packages [

	^ self packageNames collect: [ :name | IcePackage named: name repository: self repository ]
]

{ #category : #'API-history' }
IceCommit >> parent [
	
	^ self ancestors first
]

{ #category : #printing }
IceCommit >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self id asString;
		nextPutAll: ')'
]

{ #category : #'API-project' }
IceCommit >> project [
	
	self subclassResponsibility
]

{ #category : #printing }
IceCommit >> shortId [
	
	"This is just for presentation purposes.
	Don't trust this id, it is not guaranteed to be unique"
	^ self id first: 7
]

{ #category : #printing }
IceCommit >> status [

	(self repository workingCopy referenceCommits includes: self)
		ifTrue: [ 'Current' ].

	(self repository workingCopy referenceCommits
		anySatisfy: [ :each | each isNoCommit not and: [ self isAncestorOf: each ]])
			ifTrue: [ ^ 'Loaded' ].

	(self repository branch newCommitWalk includesCommit: self)
			ifTrue: [ ^ 'Not loaded' ].

	^ 'Not merged'
]

{ #category : #'API-tags' }
IceCommit >> tags [
	
	^ self repository tags select: [ :each | each commit = self ]
]

{ #category : #accessing }
IceCommit >> timeStamp [
	
	self subclassResponsibility
]

{ #category : #validating }
IceCommit >> validateIsBranch [
	"Operation not permitted in a detached commit state"
	IceNotInBranch signal
]

{ #category : #'private-changes' }
IceCommit >> versionFor: aPackage [
	
	^ IceSavedPackageVersion fromCommit: self package: aPackage
]
