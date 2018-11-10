"
A version of a package as saved in a repository. We have one of this for each commit in a repository which modifies the specified package.

Internal representation
- commit is the commit from which I loaded this version. 
- originatingCommit is the commit in which this version was first introduced, it is useful for comparing this version to another one.


"
Class {
	#name : #IceSavedPackageVersion,
	#superclass : #Object,
	#instVars : [
		'entry',
		'info',
		'commit',
		'originatingCommit',
		'package'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceSavedPackageVersion class >> forPackage: package [
	"Use fromCommit:package: instead"
	self shouldNotImplement 
]

{ #category : #'instance creation' }
IceSavedPackageVersion class >> fromCommit: commit package: package [
	^ self new
		commit: commit;
		package: package;
		yourself
]

{ #category : #comparing }
IceSavedPackageVersion >> = another [
	self species = another species ifFalse: [ ^ false ].
	
	"If I both load commits are equal, we are the same version."
	^ self commit = another commit
]

{ #category : #accessing }
IceSavedPackageVersion >> author [
	^ self info author
]

{ #category : #accessing }
IceSavedPackageVersion >> commit [
	^ commit
]

{ #category : #accessing }
IceSavedPackageVersion >> commit: anIceCommit [
	commit := anIceCommit 
]

{ #category : #accessing }
IceSavedPackageVersion >> entry [
	^ entry
]

{ #category : #accessing }
IceSavedPackageVersion >> entry: anObject [
	entry := anObject.
	entry repository: self repository
]

{ #category : #comparing }
IceSavedPackageVersion >> hash [
	^ self commit hash
]

{ #category : #'walk definition' }
IceSavedPackageVersion >> hideYourselfFromCommitWalk: commitWalk [
	commitWalk uptoCommit: self commit
]

{ #category : #accessing }
IceSavedPackageVersion >> info [
	^ info ifNil: [ info := IceMCVersionInfo new fromCommit: self commit package: package ]
]

{ #category : #accessing }
IceSavedPackageVersion >> info: anObject [
	info := anObject
]

{ #category : #comparing }
IceSavedPackageVersion >> isAncestorOf: anotherVersion [
	^ self commit isAncestorOf: anotherVersion commit

]

{ #category : #comparing }
IceSavedPackageVersion >> isAncestorOfCommitId: anotherCommitId [
	^ self commit isAncestorOf: (self repository lookupCommit: anotherCommitId)

]

{ #category : #comparing }
IceSavedPackageVersion >> isNewerThan: another [
	^ self info timeStamp > another info timeStamp
]

{ #category : #'accessing-monticello' }
IceSavedPackageVersion >> lookupSnapshot [
	
	^ commit snapshotForPackage: self package
]

{ #category : #'accessing-monticello' }
IceSavedPackageVersion >> mcPackage [
	
	^ package mcPackage
]

{ #category : #'accessing-monticello' }
IceSavedPackageVersion >> mcSnapshot [

	^ self mcVersion snapshot
]

{ #category : #'accessing-monticello' }
IceSavedPackageVersion >> mcVersion [

	^ commit mcVersionFor: self
]

{ #category : #actions }
IceSavedPackageVersion >> merge [
	self mcVersion merge

]

{ #category : #patches }
IceSavedPackageVersion >> nextVersionName [
	^ ('{1}-{2}.{3}' format: {
		self package packageName. 
		self author. 
		self info versionNumber + 1
	})

]

{ #category : #accessing }
IceSavedPackageVersion >> package [
	^ package
]

{ #category : #accessing }
IceSavedPackageVersion >> package: anObject [
	package := anObject
]

{ #category : #accessing }
IceSavedPackageVersion >> packageName [
	^ self package name
]

{ #category : #printing }
IceSavedPackageVersion >> printOn: aStream [
	aStream print: self class;
		nextPut: $(;
		nextPutAll: self package name.

"	commit ifNotNil: [ 
		aStream 
		nextPutAll: ', loaded from: ';
		nextPutAll: commit shortId ]."
		
"	originatingCommit ifNotNil: [ 
		aStream 
			nextPutAll: ', committed in: ';
			nextPutAll: originatingCommit shortId ]."
		
	aStream nextPut: $).

]

{ #category : #accessing }
IceSavedPackageVersion >> repository [
	^ self package repository
]

{ #category : #'accessing-monticello' }
IceSavedPackageVersion >> snapshot [
	"compatibility method"
	^ self mcSnapshot
]

{ #category : #patches }
IceSavedPackageVersion >> updated [
	^ self
]
