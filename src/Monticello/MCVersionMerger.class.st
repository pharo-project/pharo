"
Merge versions
"
Class {
	#name : 'MCVersionMerger',
	#superclass : 'Object',
	#instVars : [
		'records',
		'merger'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'action' }
MCVersionMerger class >> mergeVersion: aVersion [

	self new
		addVersion: aVersion;
		merge
]

{ #category : 'adding' }
MCVersionMerger >> addVersion: aVersion [
	
	records add: (MCMergeRecord version: aVersion).
	aVersion dependencies 
		do: [:ea | | dep |
			dep := ea resolve.
			(records anySatisfy: [:r | r version = dep]) 
				ifFalse: [self addVersion: dep]]
		displayingProgress: [ :ea| 'Searching dependency: ', ea package name]
]

{ #category : 'adding' }
MCVersionMerger >> addVersions: aCollection [
	aCollection do: [ :version|
		self addVersion: version].
]

{ #category : 'actions' }
MCVersionMerger >> gatherChanges [
	records do: [:ea | merger addBaseSnapshot: ea packageSnapshot].
	records do: [:ea | merger applyPatch: ea mergePatch].
]

{ #category : 'initialization' }
MCVersionMerger >> initialize [
	super initialize.
	records := OrderedCollection new.
	merger := MCThreeWayMerger new.
]

{ #category : 'actions' }
MCVersionMerger >> merge [

	self gatherChanges.

	self resolveConflicts ifTrue:
		[merger load.
		records do: [:ea | ea updateWorkingCopy].
		^ true].
	
	^ false
]

{ #category : 'actions' }
MCVersionMerger >> resolveConflicts [
	(records allSatisfy: [:ea | ea isAncestorMerge]) 
		ifTrue: [
			MCNoChangesException signal. 
			^ false].
		
	^ ((MCMergeResolutionRequest new merger: merger)
			signal: 'Merging ', records first version info name) = true
]
