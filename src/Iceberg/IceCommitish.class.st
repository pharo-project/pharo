"
I represent a repository commitish, in other words, an object that is or could be used as a commit.
I am an abstract class presenting the main API of my subclasses.

My main actions are

 - checkout: load all code of a commit into the working copy, and set itself as HEAD.
     aCommitish checkout.

 - checkoutPackageNamed: load the code of a package into the working copy, without setting HEAD.
     aCommitish checkoutPackageNamed: aPackageNameString.

 - merge: merge myself in the working copy
     aCommitish merge.

- switch: set myself as HEAD
    aCommitish switch.

I can also be queried for packages, ancestry and to make diffs to another commitishs:

 aCommitish packageNames.
 aCommitish diffTo: anotherCommitish.

 aCommitish commits.
 aCommitish newCommitWalk.
"
Class {
	#name : #IceCommitish,
	#superclass : #Object,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceCommitish class >> inRepository: aRepository [

	^ self new
		repository: aRepository;
		yourself
]

{ #category : #'API-changes' }
IceCommitish >> changesFromCommit: anIceCommit [ 

	^ self subclassResponsibility
]

{ #category : #'API-changes' }
IceCommitish >> changesTo: aCommitish [

	^ self commit changesTo: aCommitish
]

{ #category : #'API-actions' }
IceCommitish >> checkout [
		
	self checkout: (IceCheckoutAlreadyLoadedPackages new
		committish: self;
		yourself)
]

{ #category : #'API-checkout' }
IceCommitish >> checkout: aCheckoutStrategy [
	
	aCheckoutStrategy committish: self.
	^ repository checkout: aCheckoutStrategy
]

{ #category : #'API-actions' }
IceCommitish >> checkoutAllPackages [
		
	repository checkout: (IceCheckoutAllPackages new
		committish: self;
		yourself)
]

{ #category : #'API-actions' }
IceCommitish >> checkoutWithStrategy: aCheckoutStrategy [
	"Load code of a myself into the working copy (following strategy), and set myself as HEAD."
	
	repository checkout: (aCheckoutStrategy commitish: self)
]

{ #category : #accessing }
IceCommitish >> commit [
	
	self subclassResponsibility
]

{ #category : #'API-history' }
IceCommitish >> commits [
	
	^ self newCommitWalk commits
]

{ #category : #'API-history' }
IceCommitish >> commitsDo: aBlockClosure [ 
	
	self newCommitWalk commitsDo: aBlockClosure
]

{ #category : #'API-history' }
IceCommitish >> commonAncestorWith: anIceWorkingCopy [ 

	^ self commit commonAncestorWith: anIceWorkingCopy
]

{ #category : #'API-changes' }
IceCommitish >> diffTo: aCommitish [

	^ IceDiff from: self to: aCommitish
]

{ #category : #'API-packages' }
IceCommitish >> includesPackageNamed: aString [
	
	^ false
]

{ #category : #testing }
IceCommitish >> isBranch [
	
	^ false
]

{ #category : #testing }
IceCommitish >> isDetached [

	^ self isBranch not
]

{ #category : #testing }
IceCommitish >> isTag [
	
	^ false
]

{ #category : #testing }
IceCommitish >> isUnbornBranch [
	
	^ false
]

{ #category : #testing }
IceCommitish >> isWorkingCopy [
	
	^ false
]

{ #category : #'API-actions' }
IceCommitish >> loadPackageNamed: aString [ 
	"Load the code of the given package into the working copy.
	Does not modify HEAD"
		
	self repository workingCopy
		loadPackageNamed: aString
		fromCommit: self
]

{ #category : #'private-monticello' }
IceCommitish >> mcVersionFor: anIceSavedPackageVersion [ 
	
	^ repository mcVersionFor: anIceSavedPackageVersion
]

{ #category : #'API-actions' }
IceCommitish >> merge [
	
	^ self repository workingCopy mergeCommit: self commit
]

{ #category : #'API-history' }
IceCommitish >> newCommitWalk [

	self subclassResponsibility
]

{ #category : #'API-packages' }
IceCommitish >> packageNames [
	
	^ self subclassResponsibility
]

{ #category : #'API-packages' }
IceCommitish >> packages [
	
	^ #()
]

{ #category : #'API-project' }
IceCommitish >> project [
	
	self subclassResponsibility
]

{ #category : #'API-properties' }
IceCommitish >> properties [
	^ IceRepositoryProperties
		fromFileReferenceDirectory: self project sourceDirectoryReference
		commitish: self
]

{ #category : #'API-properties' }
IceCommitish >> readerClass [
	
	^ self commit readerClass
]

{ #category : #accessing }
IceCommitish >> repository [
	^ repository
]

{ #category : #accessing }
IceCommitish >> repository: aRepository [
	
	repository := aRepository
]

{ #category : #'private-monticello' }
IceCommitish >> snapshotForPackage: anIceSavedPackage [

	^ [ (self mcVersionFor: (self versionFor: anIceSavedPackage)) snapshot ]
		on: IceVersionDoesNotExist 
		do: [ MCSnapshot empty ]
]

{ #category : #'API-actions' }
IceCommitish >> switch [

	self checkout: IceCheckoutDoNotLoadPackages new
]

{ #category : #'API-actions' }
IceCommitish >> switchAndMergeWorkingCopy [
	self switch.
	self flag: #todo. "Demeters law"
	self repository workingCopy referenceCommit merge.
	self commit merge
]

{ #category : #'API-tags' }
IceCommitish >> tagNames [
	
	^ self tags collect: #name
]

{ #category : #'API-tags' }
IceCommitish >> tags [
	
	^ self commit tags
]

{ #category : #validating }
IceCommitish >> validateCanMerge [

	| imageCommit headCommit mergeCommit |
	self repository isModified
		ifTrue: [ Warning signal: 'Experimental Feature: merge when there is a dirty working copy. Could cause a loss of your local changes. Please commit before merge.' ].
	
	imageCommit := self repository workingCopy referenceCommit.
	headCommit := self repository headCommit.
	mergeCommit := self commit.
	
	{ mergeCommit . imageCommit . headCommit } asSet size > 2
		ifTrue: [ 
			IceMergeAborted signal: ('Cannot perform a tri-merge. You are trying to merge {1} into {2}, but the image is in {3}. Please, procceed to load either {1} or {2} in your image to merge.'
				format: { 
					mergeCommit description.
					self repository head description.
					imageCommit description }) ].
]

{ #category : #validating }
IceCommitish >> validateIsBranch [
	
	"Nothing. I'm a branch"
]

{ #category : #'private-monticello' }
IceCommitish >> versionFor: aPackage [

	self subclassResponsibility
]

{ #category : #'API-properties' }
IceCommitish >> writerClass [
	
	^ self commit writerClass
]
