"
I am a command to open Calypso browser on all packages from given Iceberg repository (from the head commit)
"
Class {
	#name : 'ClyBrowseIcebergRepositoryCommand',
	#superclass : 'IceTipRepositoryCommand',
	#category : 'Calypso-SystemPlugins-Monticello-Browser',
	#package : 'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : 'accessing' }
ClyBrowseIcebergRepositoryCommand class >> defaultDescription [

	^ 'Browse the version in repository'
]

{ #category : 'accessing' }
ClyBrowseIcebergRepositoryCommand class >> defaultName [

	^ 'Browse this version in Calypso'
]

{ #category : 'testing' }
ClyBrowseIcebergRepositoryCommand >> canBeExecuted [

	^ self isRepositoryOperational
]

{ #category : 'execution' }
ClyBrowseIcebergRepositoryCommand >> execute [

	| env commit snapshot |
	commit := self repositoryModel commit.
	env := RGEnvironment new.
	commit packageNames
		do: [ :packageName |
			snapshot := commit snapshotFor: (Package named: packageName).
			snapshot importInto: env asPackageNamed: packageName ]
		displayingProgress: 'loading'.
	env clean.
	env browseAs: self repository description
]

{ #category : 'accessing' }
ClyBrowseIcebergRepositoryCommand >> iconName [

	^ #glamorousBrowse
]

{ #category : 'accessing' }
ClyBrowseIcebergRepositoryCommand >> shortcutKey [

	^ $b meta
]
