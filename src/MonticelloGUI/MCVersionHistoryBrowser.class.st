"
Browser for version history
"
Class {
	#name : #MCVersionHistoryBrowser,
	#superclass : #MCTool,
	#instVars : [
		'ancestry',
		'index',
		'package',
		'infos'
	],
	#category : #MonticelloGUI
}

{ #category : #accessing }
MCVersionHistoryBrowser >> ancestry: anAncestry [
	ancestry := anAncestry
]

{ #category : #accessing }
MCVersionHistoryBrowser >> baseSnapshot [
	^ self snapshotForInfo: ancestry
]

{ #category : #'morphic ui' }
MCVersionHistoryBrowser >> defaultLabel [
	^ ancestry name, ' History'
]

{ #category : #'morphic ui' }
MCVersionHistoryBrowser >> getMenu: aMenu [
	index < 2 ifTrue: [^ aMenu].
	self fillMenu: aMenu fromSpecs: 
		(Array
			with: (Array with: 'view changes -> ', ancestry name with: #viewChanges)
			with: #('spawn history' spawnHistory)).
	^ aMenu
]

{ #category : #accessing }
MCVersionHistoryBrowser >> index [
	"Answer the value of index"

	^ index
]

{ #category : #accessing }
MCVersionHistoryBrowser >> index: anObject [
	"Set the value of index"

	index := anObject
]

{ #category : #accessing }
MCVersionHistoryBrowser >> infos [
	^ infos ifNil: [infos := ancestry withBreadthFirstAncestors]
]

{ #category : #'morphic ui' }
MCVersionHistoryBrowser >> initialExtent [
	^ 440@169.
	
]

{ #category : #accessing }
MCVersionHistoryBrowser >> list [
	^ self infos collect: [:ea | ea name]
]

{ #category : #accessing }
MCVersionHistoryBrowser >> package: aMCPackage [
	package := aMCPackage
]

{ #category : #accessing }
MCVersionHistoryBrowser >> repositoryGroup [
	^ MCRepositoryGroup default
]

{ #category : #accessing }
MCVersionHistoryBrowser >> selectedInfo [
	^ self infos at: self selection ifAbsent: [nil]
]

{ #category : #accessing }
MCVersionHistoryBrowser >> selectedSnapshot [
	^ self snapshotForInfo: self selectedInfo
]

{ #category : #accessing }
MCVersionHistoryBrowser >> selection [
	^ index ifNil: [0]
]

{ #category : #accessing }
MCVersionHistoryBrowser >> selection: aNumber [
	index := aNumber.
	self changed: #selection; changed: #summary
]

{ #category : #accessing }
MCVersionHistoryBrowser >> snapshotForInfo: aVersionInfo [
	^ (self repositoryGroup versionWithInfo: aVersionInfo) snapshot
]

{ #category : #'morphic ui' }
MCVersionHistoryBrowser >> spawnHistory [
	MCVersionHistoryBrowser new
		ancestry: self selectedInfo;
		package: package;
		show
]

{ #category : #accessing }
MCVersionHistoryBrowser >> summary [
	| selInfo |
	selInfo := self selectedInfo.
	^ selInfo 
		ifNil: ['']
		ifNotNil: [selInfo summary]
]

{ #category : #'morphic ui' }
MCVersionHistoryBrowser >> widgetSpecs [
	^ #(
		((listMorph:selection:menu: list selection getMenu:) (0 0 0.3 1))
		((textMorph: summary) (0.3 0 1 1))
	 	)
]
