"
I am a refactory change for a package creating. The RBRefactory api is implemented in my super class, I just define the concrete ""package"" creating.
"
Class {
	#name : 'ReAddPackageChange',
	#superclass : 'RePackageChange',
	#instVars : [
		'packageName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'adding' }
ReAddPackageChange class >> addPackageNamed: aString [

	^ self new addPackageNamed: aString
]

{ #category : 'adding' }
ReAddPackageChange >> addPackageNamed: aString [
	packageName := aString
]

{ #category : 'converting' }
ReAddPackageChange >> asUndoOperation [
	^ changeFactory removePackageNamed: packageName
]

{ #category : 'printing' }
ReAddPackageChange >> changeString [

	^ String streamContents: [ :aStream |
			aStream
				nextPutAll: 'self packageOrganizer ';
				nextPutAll: #ensurePackage:;
				nextPutAll: packageName ]
]

{ #category : 'private' }
ReAddPackageChange >> generateChanges [

	self packageOrganizer ensurePackage: packageName
]
