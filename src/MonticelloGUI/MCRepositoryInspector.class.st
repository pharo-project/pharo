"
Inspector for a repository
"
Class {
	#name : #MCRepositoryInspector,
	#superclass : #MCVersionInspector,
	#instVars : [
		'repository',
		'packages',
		'versions',
		'selectedPackage',
		'selectedVersion'
	],
	#category : #MonticelloGUI
}

{ #category : #'as yet unclassified' }
MCRepositoryInspector class >> repository: aFileBasedRepository workingCopy: aWorkingCopy [
	^self new
		setRepository: aFileBasedRepository workingCopy: aWorkingCopy;
		yourself
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> buttonSpecs [
	^#(('Refresh' refresh 'refresh the version-list')) , super buttonSpecs
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> defaultLabel [
	^'Repository: ' , repository description
]

{ #category : #testing }
MCRepositoryInspector >> hasVersion [
	^ selectedVersion notNil
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> initialExtent [
	^450@300
]

{ #category : #actions }
MCRepositoryInspector >> load [
	self hasVersion
		ifFalse: [ ^ self ].
	super load.
	self version workingCopy repositoryGroup addRepository: repository
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> packageList [
	^ packages collect: [:ea | ea name]
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> packageListMenu: aMenu [
	^aMenu
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> packageSelection [
	^ packages indexOf: selectedPackage
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> packageSelection: aNumber [
	selectedPackage := aNumber isZero ifFalse: [ packages at: aNumber ].
	versions := repository versionsAvailableForPackage: selectedPackage.
	self changed: #packageSelection; changed: #versionList
]

{ #category : #actions }
MCRepositoryInspector >> refresh [
	packages := repository packages.
	self changed: #packageList.
	self packageSelection: self packageSelection.

]

{ #category : #accessing }
MCRepositoryInspector >> setRepository: aRepository workingCopy: aWorkingCopy [
	repository := aRepository.
	aWorkingCopy ifNotNil: [ selectedPackage := aWorkingCopy package].
	self refresh
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> sortedVersions [
	| sorter |
	sorter := MCVersionSorter new.
	sorter addAllVersionInfos: versions.
	^ sorter sortedVersionInfos select: [:ea | versions includes: ea]
]

{ #category : #accessing }
MCRepositoryInspector >> summary [
	^ selectedVersion
		ifNotNil: [selectedVersion summary]
		ifNil: ['']
]

{ #category : #accessing }
MCRepositoryInspector >> version [
	^ version ifNil: [version := repository versionWithInfo: selectedVersion]
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> versionList [
	^ self sortedVersions collect: [:ea | ea name]
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> versionListMenu: aMenu [
	^aMenu
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> versionSelection [
	^ versions indexOf: selectedVersion
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> versionSelection: aNumber [
	selectedVersion := aNumber isZero
		ifTrue: [ nil ]
		ifFalse: [ versions at: aNumber ].
	self
		changed: #versionSelection;
		changed: #summary
]

{ #category : #'morphic ui' }
MCRepositoryInspector >> widgetSpecs [
	^#(	((buttonRow) (0 0 1 0) (0 0 0 30))
		((listMorph: package) (0 0 0.5 0.6) (0 30 0 0))
		((listMorph: version) (0.5 0 1 0.6) (0 30 0 0))
		((textMorph: summary) (0 0.6 1 1) (0 0 0 0)) )
]
