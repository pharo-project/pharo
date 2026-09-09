"
I am a refactory change for a package removing. The RBRefactory api is implemented in my super class, I just define the concrete ""package"" removing.
"
Class {
	#name : 'ReRemovePackageChange',
	#superclass : 'RePackageChange',
	#instVars : [
		'packageName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'removing' }
ReRemovePackageChange class >> removePackageNamed: aString [

	^ self new removePackageNamed: aString
]

{ #category : 'converting' }
ReRemovePackageChange >> asUndoOperation [

	^ changeFactory addPackageNamed: packageName
]

{ #category : 'printing' }
ReRemovePackageChange >> changeString [

	^ String streamContents: [ :aStream |
			aStream
				nextPutAll: '(self packageOrganizer packageNamed: ';
				nextPutAll: packageName;
				nextPutAll: ') removeFromSystem' ]
]

{ #category : 'private' }
ReRemovePackageChange >> generateChanges [

	self package ifNotNil: [ :package | package removeFromSystem ]
]

{ #category : 'accessing' }
ReRemovePackageChange >> package [

	^ (browserEnvironment packageAt: packageName ifAbsent: [nil])
]

{ #category : 'removing' }
ReRemovePackageChange >> removePackageNamed: aString [

	packageName := aString
]
