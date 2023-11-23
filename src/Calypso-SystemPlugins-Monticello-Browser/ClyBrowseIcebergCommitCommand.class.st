"
I am a command to open Calypso browser on all packages from given Iceberg commit
"
Class {
	#name : 'ClyBrowseIcebergCommitCommand',
	#superclass : 'IceTipCommand',
	#category : 'Calypso-SystemPlugins-Monticello-Browser',
	#package : 'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : 'accessing' }
ClyBrowseIcebergCommitCommand class >> defaultDescription [

	^ 'Browse the version in repository'
]

{ #category : 'accessing' }
ClyBrowseIcebergCommitCommand class >> defaultName [

	^ 'Browse this version in Calypso'
]

{ #category : 'execution' }
ClyBrowseIcebergCommitCommand >> execute [

	| env snapshot methodHistoryLogEntry |
	env := RGEnvironment new.
	methodHistoryLogEntry := self methodHistoryLogEntryModel entity.
	methodHistoryLogEntry commit packageNames
		do: [ :packageName |
			snapshot := methodHistoryLogEntry commit snapshotFor: (Package named: packageName).
			snapshot importInto: env asPackageNamed: packageName ]
		displayingProgress: 'loading'.
	env clean.
	env browseAs: methodHistoryLogEntry commit repository origin url , '[', methodHistoryLogEntry shortId, ']'
]

{ #category : 'accessing' }
ClyBrowseIcebergCommitCommand >> iconName [

	^ #glamorousBrowse
]

{ #category : 'execution' }
ClyBrowseIcebergCommitCommand >> methodHistoryLogEntryModel [

	^ self item
]

{ #category : 'accessing' }
ClyBrowseIcebergCommitCommand >> shortcutKey [

	^ $b meta
]
