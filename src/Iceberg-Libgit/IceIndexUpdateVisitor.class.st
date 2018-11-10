Class {
	#name : #IceIndexUpdateVisitor,
	#superclass : #IceTreeVisitor,
	#instVars : [
		'diff',
		'index'
	],
	#category : #'Iceberg-Libgit-Commit'
}

{ #category : #accessing }
IceIndexUpdateVisitor >> diff: anIceDiff [ 
	diff := anIceDiff
]

{ #category : #accessing }
IceIndexUpdateVisitor >> index: anIndex [
	
	index := anIndex
]

{ #category : #accessing }
IceIndexUpdateVisitor >> repository [

	^ index repository
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitAddition: anIceAddition [ 
	
	anIceAddition definition accept: self
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitDirectoryDefinition: anIceDirectoryDefinition [ 
	self visitChildrenOf: currentNode
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitFileNode: anIceFileDefinition [ 
	
	index addPath: anIceFileDefinition path
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitModification: anIceModification [ 
	
	^ anIceModification leftDefinition accept: self
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitNoModification: anIceNoModification [ 
	
	anIceNoModification definition accept: self
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitNonConflictingOperation: anIceNonConflictingOperation [ 
	
	anIceNonConflictingOperation chosenOperation accept: self
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitPackage: anIcePackageDefinition [ 
	
	index addPackage: anIcePackageDefinition
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitRemoval: anIceRemoval [

	"Do the same as an addition"
	self visitAddition: anIceRemoval
]

{ #category : #visiting }
IceIndexUpdateVisitor >> visitRootDefinition: anIceRootDefinition [ 
	
	self visitChildrenOf: currentNode
]
