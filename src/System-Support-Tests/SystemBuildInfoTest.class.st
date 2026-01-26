Class {
	#name : 'SystemBuildInfoTest',
	#superclass : 'TestCase',
	#category : 'System-Support-Tests-Image',
	#package : 'System-Support-Tests',
	#tag : 'Image'
}

{ #category : 'tests' }
SystemBuildInfoTest >> testIsBuildFinished [

	| buildInfo |
	buildInfo := SystemBuildInfo new.

	self deny: buildInfo isBuildFinished.

	buildInfo initializeForRelease.

	self assert: buildInfo isBuildFinished
]

{ #category : 'tests' }
SystemBuildInfoTest >> testSystemPackageNames [

	| buildInfo newPackage |
	buildInfo := SystemBuildInfo new.
	buildInfo initializeForRelease.

	self denyEmpty: buildInfo systemPackageNames.
	buildInfo systemPackageNames do: [ :package | self assert: (self packageOrganizer packageNames includes: package) ].

	[
	newPackage := self packageOrganizer ensurePackage: 'NewPackageNotPartOfPharo'.

	self deny: (buildInfo systemPackageNames includes: newPackage name) ] ensure: [ self packageOrganizer removePackage: 'NewPackageNotPartOfPharo' ]
]

{ #category : 'tests' }
SystemBuildInfoTest >> testSystemPackages [

	| buildInfo newPackage |
	buildInfo := SystemBuildInfo new.
	buildInfo initializeForRelease.

	self denyEmpty: buildInfo systemPackages.
	buildInfo systemPackages do: [ :package |
		self assert: package isPackage.
		self assert: (self packageOrganizer packages includes: package) ].

	[
	newPackage := self packageOrganizer ensurePackage: 'NewPackageNotPartOfPharo'.

	self deny: (buildInfo systemPackages includes: newPackage) ] ensure: [ self packageOrganizer removePackage: 'NewPackageNotPartOfPharo' ]
]
