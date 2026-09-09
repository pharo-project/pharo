Class {
	#name : 'MCMczInstallerTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'expected',
		'diff'
	],
	#category : 'Monticello-Tests',
	#package : 'Monticello-Tests'
}

{ #category : 'assertions' }
MCMczInstallerTest >> assertDict: dict matchesInfo: info [
	#(name id message date time author) do: [ :sel | (info perform: sel) ifNotNil: [ :i | dict at: sel ifPresent: [ :d | self assert: i equals: d ] ] ].
	info ancestors with: (dict at: #ancestors) do: [ :i :d | self assertDict: d matchesInfo: i ]
]

{ #category : 'assertions' }
MCMczInstallerTest >> assertNoChange [
	| actual |
	actual := MCSnapshotResource takeSnapshot.
	diff := actual patchRelativeToBase: expected snapshot.
	self assertEmpty: diff
]

{ #category : 'accessing' }
MCMczInstallerTest >> fileName [
	^ 'InstallerTest.mcz'
]

{ #category : 'running' }
MCMczInstallerTest >> setUp [
	super setUp.
	expected := self mockVersion.
	self change: #one toReturn: 2
]

{ #category : 'running' }
MCMczInstallerTest >> tearDown [

	expected snapshot updatePackage: self mockPackage.
	self fileName asFileReference ensureDelete.
	super tearDown
]

{ #category : 'tests' }
MCMczInstallerTest >> testInstallFromFile [

	self fileName asFileReference
		ensureDelete;
		binaryWriteStreamDo: [ :stream | MCMczWriter fileOut: expected on: stream ].

	MczInstaller installFileNamed: self fileName.
	self assertNoChange
]

{ #category : 'tests' }
MCMczInstallerTest >> testInstallFromStream [

	| array |
	array := ByteArray streamContents: [ :stream | MCMczWriter fileOut: expected on: stream ].
	MczInstaller installStream: array readStream.
	self assertNoChange.
	self assertDict: (MczInstaller versionInfo at: self mockPackage name) matchesInfo: expected info
]
