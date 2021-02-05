Class {
	#name : #RBRenamePackageChange,
	#superclass : #RBRefactoryPackageChange,
	#instVars : [
		'oldName',
		'newName'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #'instance creation' }
RBRenamePackageChange class >> rename: oldString to: newString [ 
	^ self new
		rename: oldString to: newString;
		yourself
]

{ #category : #converting }
RBRenamePackageChange >> asUndoOperation [
	^ changeFactory renamePackageNamed: newName to: oldName
]

{ #category : #accessing }
RBRenamePackageChange >> changePackage [
	^ (RBBrowserEnvironment default packageAt: oldName ifAbsent: [nil]) 
		ifNil: [ RBBrowserEnvironment default packageAt: newName ifAbsent: [nil] ]
]

{ #category : #private }
RBRenamePackageChange >> primitiveExecute [
	self changePackage renameTo: newName.
]

{ #category : #printing }
RBRenamePackageChange >> printOn: aStream [ 
	aStream
		nextPutAll: oldName;
		nextPutAll: ' renameTo: ';
		print: newName;
		nextPut: $!
]

{ #category : #'instance creation' }
RBRenamePackageChange >> rename: oldString to: newString [ 
	oldName := oldString.
	newName := newString
]
