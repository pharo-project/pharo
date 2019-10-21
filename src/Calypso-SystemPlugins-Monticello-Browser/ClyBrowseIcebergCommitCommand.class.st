"
I am a command to open Calypso browser on all packages from given Iceberg commit
"
Class {
	#name : #ClyBrowseIcebergCommitCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'commitModel'
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
	| env snapshot commit |
	env := RGEnvironment new.
	commit := commitModel entity.
	commit packageNames 
		do: [ :packageName |
			snapshot := commit snapshotFor: (RPackage named: packageName).
			snapshot importInto: env asPackageNamed: packageName ] 
		displayingProgress: 'loading'.
	env clean.
	env browseAs: commit repository origin url , '[', commit shortId, ']'
]

{ #category : #execution }
ClyBrowseIcebergCommitCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	commitModel := aToolContext item
]
