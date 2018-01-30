Class {
	#name : #MCMczInstallerTest,
	#superclass : #MCTestCase,
	#instVars : [
		'expected',
		'diff'
	],
	#category : #'Monticello-Tests-IO'
}

{ #category : #testing }
MCMczInstallerTest class >> isAbstract [
	^ (Smalltalk hasClassNamed: #MczInstaller) not
		
]

{ #category : #accessing }
MCMczInstallerTest class >> suite [
	^ (Smalltalk hasClassNamed: #MczInstaller)
		ifTrue: [ super suite ]
		ifFalse: [ self classForTestSuite new name: self name asString ]
]

{ #category : #assertions }
MCMczInstallerTest >> assertDict: dict matchesInfo: info [
	#(name id message date time author)
		do: [:sel |  (info perform: sel) ifNotNil: [:i | dict at: sel ifPresent: [:d | self assert: i = d]]].
	info ancestors 
			with: (dict at: #ancestors) 
			do: [:i :d | self assertDict: d matchesInfo: i]
]

{ #category : #assertions }
MCMczInstallerTest >> assertNoChange [
	| actual |
	actual := MCSnapshotResource takeSnapshot.
	diff := actual patchRelativeToBase: expected snapshot.
	self assert: diff isEmpty
]

{ #category : #assertions }
MCMczInstallerTest >> assertVersionInfoPresent [
	| dict info |
	dict := MczInstaller versionInfo at: self mockPackage name.
	info := expected info.
	self assertDict: dict matchesInfo: info.
]

{ #category : #utilities }
MCMczInstallerTest >> deleteFile [
	self fileName asFileReference ensureDelete
]

{ #category : #accessing }
MCMczInstallerTest >> fileName [
	^ 'InstallerTest.mcz'
]

{ #category : #accessing }
MCMczInstallerTest >> fileStream [
	^ FileStream forceNewFileNamed: self fileName.
]

{ #category : #running }
MCMczInstallerTest >> setUp [
	super setUp.
	expected := self mockVersion.
	self change: #one toReturn: 2.
]

{ #category : #running }
MCMczInstallerTest >> tearDown [
	expected snapshot updatePackage: self mockPackage.
	self deleteFile.
	super tearDown
]

{ #category : #tests }
MCMczInstallerTest >> testInstallFromFile [
	MCMczWriter fileOut: expected on: self fileStream.
	MczInstaller installFileNamed: self fileName.
	self assertNoChange.
]

{ #category : #tests }
MCMczInstallerTest >> testInstallFromStream [
	| stream |
	stream := RWBinaryOrTextStream on: String new.
	MCMczWriter fileOut: expected on: stream.
	MczInstaller installStream: stream reset.
	self assertNoChange.
	self assertVersionInfoPresent.
	
]
