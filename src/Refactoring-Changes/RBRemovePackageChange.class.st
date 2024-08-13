"
I am a refactory change for a package removing. The RBRefactory api is implemented in my super class, I just define the concrete ""package"" removing.
"
Class {
	#name : 'RBRemovePackageChange',
	#superclass : 'RBRefactoryPackageChange',
	#instVars : [
		'packageName'
	],
	#category : 'Refactoring-Changes-Changes-Packages',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Packages'
}

{ #category : 'removing' }
RBRemovePackageChange class >> removePackageNamed: aString [

	^ self new removePackageNamed: aString
]

{ #category : 'converting' }
RBRemovePackageChange >> asUndoOperation [

	^ changeFactory addPackageNamed: packageName
]

{ #category : 'private' }
RBRemovePackageChange >> generateChanges [

	self package ifNotNil: [ :package | package removeFromSystem ]
]

{ #category : 'accessing' }
RBRemovePackageChange >> package [

	^ (browserEnvironment packageAt: packageName ifAbsent: [nil])
]

{ #category : 'removing' }
RBRemovePackageChange >> removePackageNamed: aString [

	packageName := aString
]
