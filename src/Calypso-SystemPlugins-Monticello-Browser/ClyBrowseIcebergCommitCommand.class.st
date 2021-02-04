"
I am a command to open Calypso browser on all packages from given Iceberg commit
"
Class {
	#name : #ClyBrowseIcebergCommitCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'methodHistoryLogEntryModel'
	],
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #activation }
ClyBrowseIcebergCommitCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
ClyBrowseIcebergCommitCommand class >> browserHistoryShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $b meta for: IceTipHistoryContext
]

{ #category : #accessing }
ClyBrowseIcebergCommitCommand class >> defaultHelp [
	^ 'Browse the version in repository'
]

{ #category : #accessing }
ClyBrowseIcebergCommitCommand class >> defaultMenuIconName [
	^ #glamorousBrowse
]

{ #category : #accessing }
ClyBrowseIcebergCommitCommand class >> defaultMenuItemName [
	^ 'Browse repository version'
]

{ #category : #execution }
ClyBrowseIcebergCommitCommand >> execute [
	| env snapshot methodHistoryLogEntry |
	env := RGEnvironment new.
	methodHistoryLogEntry := methodHistoryLogEntryModel entity.
	methodHistoryLogEntry commit packageNames
		do: [ :packageName |
			snapshot := methodHistoryLogEntry commit snapshotFor: (RPackage named: packageName).
			snapshot importInto: env asPackageNamed: packageName ] 
		displayingProgress: 'loading'.
	env clean.
	env browseAs: methodHistoryLogEntry commit repository origin url , '[', methodHistoryLogEntry shortId, ']'
]

{ #category : #execution }
ClyBrowseIcebergCommitCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	methodHistoryLogEntryModel := aToolContext item
]
