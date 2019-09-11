"
A record for merging
"
Class {
	#name : #MCMergeRecord,
	#superclass : #Object,
	#instVars : [
		'version',
		'packageSnapshot',
		'ancestorInfo',
		'ancestorSnapshot',
		'imagePatch',
		'mergePatch'
	],
	#category : #'Monticello-Versioning'
}

{ #category : #'instance creation' }
MCMergeRecord class >> version: aVersion [
	^ self basicNew initializeWithVersion: aVersion
]

{ #category : #accessing }
MCMergeRecord >> ancestorInfo [
	^ ancestorInfo ifNil: [ancestorInfo := version info commonAncestorWith: version workingCopy ancestry]
]

{ #category : #accessing }
MCMergeRecord >> ancestorSnapshot [
	^ ancestorSnapshot ifNil: [ancestorSnapshot := version workingCopy findSnapshotWithVersionInfo: self ancestorInfo]
]

{ #category : #testing }
MCMergeRecord >> imageIsClean [
	| ancestors |
	ancestors := version workingCopy ancestors.
	^ ancestors size = 1
		and: [(ancestors first = self ancestorInfo)
				and: [self imagePatch isEmpty]]
]

{ #category : #accessing }
MCMergeRecord >> imagePatch [
	^ imagePatch ifNil: [imagePatch := self packageSnapshot patchRelativeToBase: self ancestorSnapshot]
]

{ #category : #initialization }
MCMergeRecord >> initializeWithVersion: aVersion [
	version := aVersion
]

{ #category : #testing }
MCMergeRecord >> isAncestorMerge [
	^ version workingCopy ancestry hasAncestor: version info
]

{ #category : #accessing }
MCMergeRecord >> mergePatch [
	^ mergePatch ifNil: [mergePatch := version snapshot patchRelativeToBase: self ancestorSnapshot]
]

{ #category : #accessing }
MCMergeRecord >> packageSnapshot [
	^ packageSnapshot ifNil: [packageSnapshot := version package snapshot]
]

{ #category : #actions }
MCMergeRecord >> updateWorkingCopy [
	self isAncestorMerge ifFalse:
		[self imageIsClean
			ifTrue: [version workingCopy loaded: version]
			ifFalse: [version workingCopy merged: version]]
]

{ #category : #accessing }
MCMergeRecord >> version [
	^ version
]
