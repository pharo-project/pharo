Class {
	#name : 'SycFileOutPackageCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-PackageCommands-Tests-Tests',
	#package : 'SystemCommands-PackageCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycFileOutPackageCommandTest >> testDefaultFileNameForMultiplePackages [
	| cmd pkg1 pkg2 |
	cmd := SycFileOutPackageCommand new.
	pkg1 := self packageOrganizer packageNamed: 'CodeExport'.
	pkg2 := self packageOrganizer packageNamed: 'Kernel'.
	cmd packages: { pkg1 . pkg2 }.
	self assert: cmd defaultFileName equals: 'packagefileout'
]

{ #category : 'tests' }
SycFileOutPackageCommandTest >> testDefaultFileNameForSinglePackage [
	| cmd pkg |
	cmd := SycFileOutPackageCommand new.
	pkg := self packageOrganizer packageNamed: 'CodeExport'.
	cmd packages: { pkg }.
	self assert: cmd defaultFileName equals: 'CodeExport'
]
