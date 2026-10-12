Class {
	#name : 'SystemBuildInfoTest',
	#superclass : 'TestCase',
	#instVars : [
		'buildInfo'
	],
	#category : 'System-Support-Tests-Image',
	#package : 'System-Support-Tests',
	#tag : 'Image'
}

{ #category : 'running' }
SystemBuildInfoTest >> setUp [

	super setUp.
	buildInfo := SystemBuildInfo new
		        initializeForRelease;
		        yourself
]

{ #category : 'tests' }
SystemBuildInfoTest >> testIsBuildFinished [

	buildInfo := SystemBuildInfo new.
	self deny: buildInfo isBuildFinished.

	buildInfo initializeForRelease.

	self assert: buildInfo isBuildFinished
]

{ #category : 'tests' }
SystemBuildInfoTest >> testIsSystemPackage [

	| newPackage |
	self assert: (buildInfo systemPackagesIncludes: self class package).
	newPackage := self packageOrganizer ensurePackage: 'GeneratedPackageForTestsOfSystemBuildInfo'.
	[ self deny: (buildInfo systemPackagesIncludes: newPackage) ] ensure: [ newPackage ifNotNil: [ :package | package removeFromSystem ] ]
]

{ #category : 'tests' }
SystemBuildInfoTest >> testSystemPackageNames [

	| newPackage |
	self denyEmpty: buildInfo systemPackageNames.
	buildInfo systemPackageNames do: [ :package | self assert: (self packageOrganizer packageNames includes: package) ].

	[
		newPackage := self packageOrganizer ensurePackage: 'NewPackageNotPartOfPharo'.

		self deny: (buildInfo systemPackageNames includes: newPackage name) ] ensure: [ self packageOrganizer removePackage: 'NewPackageNotPartOfPharo' ]
]

{ #category : 'tests' }
SystemBuildInfoTest >> testSystemPackages [

	| newPackage |
	self denyEmpty: buildInfo systemPackages.
	buildInfo systemPackages do: [ :package |
			self assert: package isPackage.
			self assert: (self packageOrganizer packages includes: package) ].

	[
		newPackage := self packageOrganizer ensurePackage: 'NewPackageNotPartOfPharo'.

		self deny: (buildInfo systemPackages includes: newPackage) ] ensure: [ self packageOrganizer removePackage: 'NewPackageNotPartOfPharo' ]
]
