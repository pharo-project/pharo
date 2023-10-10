"
I am a refactory change for a package creating. The RBRefactory api is implemented in my super class, I just define the concrete ""package"" creating.
"
Class {
	#name : 'RBAddPackageChange',
	#superclass : 'RBRefactoryPackageChange',
	#instVars : [
		'packageName'
	],
	#category : 'Refactoring-Changes-Changes-Packages',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Packages'
}

{ #category : 'adding' }
RBAddPackageChange class >> addPackageNamed: aString [

	^ self new addPackageNamed: aString
]

{ #category : 'adding' }
RBAddPackageChange >> addPackageNamed: aString [
	packageName := aString
]

{ #category : 'converting' }
RBAddPackageChange >> asUndoOperation [
	^ changeFactory removePackageNamed: packageName
]

{ #category : 'private' }
RBAddPackageChange >> generateChanges [

	self packageOrganizer ensurePackage: packageName
]

{ #category : 'printing' }
RBAddPackageChange >> printOn: aStream [
	aStream
		nextPutAll: 'self packageOrganizer ';
		nextPutAll: #ensurePackage:;
		nextPutAll: packageName;
		nextPut: $!
]
