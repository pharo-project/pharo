"
I visit a tree of changes and apply those changes to the working copy.
"
Class {
	#name : #IceWorkingCopyPatcherVisitor,
	#superclass : #IceTreeVisitor,
	#instVars : [
		'repository',
		'loader'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceWorkingCopyPatcherVisitor class >> on: aRepository [
	^ self new 
		repository: aRepository;
		yourself
]

{ #category : #initialization }
IceWorkingCopyPatcherVisitor >> initialize [
	super initialize.
	loader := MCPackageLoader new
]

{ #category : #loading }
IceWorkingCopyPatcherVisitor >> load [
	loader load
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> remoteFileReferenceInPath: anUndefinedObject [ 
	
	"I do not remove nothing from the file in the image"
]

{ #category : #actions }
IceWorkingCopyPatcherVisitor >> removeClass: anIceClassDefinition [ 
	
	repository workingCopy removeClassNamed: anIceClassDefinition name
]

{ #category : #removing }
IceWorkingCopyPatcherVisitor >> removeFile: anIceFileDefinition [ 
	"I do not remove nothing from the file in the image"
]

{ #category : #actions }
IceWorkingCopyPatcherVisitor >> removeMethod: anIceMethodDefinition [ 

	repository workingCopy removeMethod: anIceMethodDefinition
]

{ #category : #operations }
IceWorkingCopyPatcherVisitor >> removePackage: anIcePackageDefinition [ 
	
	repository workingCopy unloadPackageNamed: anIcePackageDefinition name
]

{ #category : #accessing }
IceWorkingCopyPatcherVisitor >> repository [
	^ repository
]

{ #category : #accessing }
IceWorkingCopyPatcherVisitor >> repository: anObject [
	repository := anObject
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitAddition: anIceAddition [ 
	
	repository workingCopy applyDefinition: anIceAddition definition inNode: currentNode
]

{ #category : #'as yet unclassified' }
IceWorkingCopyPatcherVisitor >> visitClassDefinition: anIceClassDefinition [ 
	
	repository workingCopy applyDefinition: anIceClassDefinition inNode: currentNode.
	self visitChildrenOf: currentNode.
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitDirectoryDefinition: anIceDirectoryDefinition [

	self visitChildrenOf: currentNode
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitFileNode: anIceFileDefinition [
	
	"Ignore files, they are not loaded in the image"
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitMethodNode: anIceMethodDefinition [ 

	repository workingCopy applyDefinition: anIceMethodDefinition inNode: currentNode
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitModification: anIceModification [ 
	
	anIceModification definition accept: self
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitNoModification: anIceNoModification [ 

	anIceNoModification definition accept: self
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitNonConflictingOperation: anIceNonConflictingOperation [ 
	
	^ anIceNonConflictingOperation chosenOperation accept: self
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitPackage: anIcePackageDefinition [
	
	(self repository packageNamed: anIcePackageDefinition name) isLoaded
		ifFalse: [ ^ self ].
	
	currentNode accept: (IceMCSnapshotPatchVisitor new
		patcher: loader;
		yourself).
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitRemoval: anIceRemoval [ 
	
	anIceRemoval definition removeFrom: self.
	self visitChildrenOf: currentNode
]

{ #category : #visiting }
IceWorkingCopyPatcherVisitor >> visitRootDefinition: anIceRootDefinition [ 
	
	self visitChildrenOf: currentNode
]
