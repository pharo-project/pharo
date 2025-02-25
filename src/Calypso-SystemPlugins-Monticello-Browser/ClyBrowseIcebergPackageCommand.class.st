"
I am a command to open Calypso browser on given package from given Iceberg repository
"
Class {
	#name : 'ClyBrowseIcebergPackageCommand',
	#superclass : 'IceTipPackageCommand',
	#category : 'Calypso-SystemPlugins-Monticello-Browser',
	#package : 'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : 'accessing' }
ClyBrowseIcebergPackageCommand class >> defaultDescription [

	^ 'Browse the version in the repository'
]

{ #category : 'accessing' }
ClyBrowseIcebergPackageCommand class >> defaultName [

	^ 'Browse this version in Calypso'
]

{ #category : 'execution' }
ClyBrowseIcebergPackageCommand >> execute [

	| env commit snapshot |
	env := RGEnvironment new.
	commit := self repositoryModel commit.

	#(1) do: [ :i |
			snapshot := commit snapshotFor: (Package named: self packageModel name).
 			snapshot importInto: env asPackageNamed: self packageModel name]
		displayingProgress: 'loading'.
	env clean.
	env browseAs: self repository description
]

{ #category : 'accessing' }
ClyBrowseIcebergPackageCommand >> iconName [

	^ #browse
]

{ #category : 'accessing' }
ClyBrowseIcebergPackageCommand >> shortcutKey [

	^ $b meta
]
