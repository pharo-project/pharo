"
I am a refactory change for a package renaming. The RBRefactory api is implemented in my super class, I just define the concrete ""package"" renaming.
"
Class {
	#name : 'ReRenamePackageChange',
	#superclass : 'RePackageChange',
	#instVars : [
		'oldName',
		'newName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRenamePackageChange class >> rename: oldString to: newString [

	^ self new
		  rename: oldString to: newString;
		  yourself
]

{ #category : 'converting' }
ReRenamePackageChange >> asUndoOperation [

	^ changeFactory renamePackageNamed: newName to: oldName
]

{ #category : 'accessing' }
ReRenamePackageChange >> changePackage [

	^ (browserEnvironment packageAt: oldName ifAbsent: [ nil ]) ifNil: [
		  browserEnvironment packageAt: newName ifAbsent: [ nil ] ]
]

{ #category : 'printing' }
ReRenamePackageChange >> changeString [

	^ String streamContents: [ :aStream |
			aStream
				nextPutAll: oldName;
				nextPutAll: ' renameTo: ';
				print: newName ]
]

{ #category : 'private' }
ReRenamePackageChange >> generateChanges [

	self changePackage renameTo: newName
]

{ #category : 'instance creation' }
ReRenamePackageChange >> rename: oldString to: newString [

	oldName := oldString.
	newName := newString
]
