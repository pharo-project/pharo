"
I am a diff between two Iceberg Commitish's. I calculate a diff by:

 - Asking to the repository the list of changed files/packages between the two versions. These are obtained, for example, by the Monticello dirty flags and the list of modified files provided by Git.
 - Using that list, the diff calculates two trees of IceDefinition. Those trees are represented as compositions of IceNode.
 - Then, the two trees are diff'd (see #diff:with:), and a tree of differences is obtained. This tree is also a composition of IceNodes, but contains IceOperation objects instead (additions, deletions and modifications).

The diff is calculated from left to right. This means that the resulting operations are the operations to apply on the right tree (the destination) to obtain the same tree as in the left (the source).

I am created by doing one of the following:

- IceDiff from: aVersion to: anotherVersion.

- IceDiff new
	sourceVersion: aVersion;
	targetVersion: anotherVersion;
	build;
	yourself
"
Class {
	#name : #IceDiff,
	#superclass : #Object,
	#instVars : [
		'tree',
		'source',
		'target',
		'writerClass'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceDiff class >> from: sourceCommitish to: targetCommitish [
	
	^ self new
		sourceVersion: sourceCommitish;
		targetVersion: targetCommitish;
		build;
		yourself
]

{ #category : #visiting }
IceDiff >> accept: aVisitor [
	
	^ tree accept: aVisitor
]

{ #category : #building }
IceDiff >> build [

	| leftTree rightTree |
	leftTree := IceNode value: IceRootDefinition new.
	rightTree := IceNode value: IceRootDefinition new.

	(source changesTo: target) do: [ :change | 
		change accept: (IceChangeImporter new
			version: source;
			diff: self;
			parentNode: leftTree;
			yourself).
			
		change accept: (IceChangeImporter new
			version: target;
			diff: self;
			parentNode: rightTree;
			yourself) ].

	tree := self diff: leftTree with: rightTree
]

{ #category : #building }
IceDiff >> buildForPackages: aCollection [

	| leftTree rightTree |

	leftTree := IceNode value: IceRootDefinition new.
	rightTree := IceNode value: IceRootDefinition new.
	
	(aCollection collect: [ :package | IceImageChange package: package ])
		do: [ :change | 
			change accept: (IceChangeImporter new
				version: source;
				diff: self;
				parentNode: leftTree;
				yourself).
				
			change accept: (IceChangeImporter new
				version: target;
				diff: self;
				parentNode: rightTree;
				yourself) ].

	tree := self diff: leftTree with: rightTree
]

{ #category : #building }
IceDiff >> buildFull [

	| leftTree rightTree |
	leftTree := IceNode value: IceRootDefinition new.
	rightTree := IceNode value: IceRootDefinition new.
		
	"Image changes should be processed before commit changes to take into account changes in the image"
	self imageChanges , (source referenceCommit changesTo: target) asArray
			do: [ :change | 
			change accept: (IceChangeImporter new
				version: source;
				diff: self;
				parentNode: leftTree;
				yourself).
				
			change accept: (IceChangeImporter new
				version: target;
				diff: self;
				parentNode: rightTree;
				yourself) ] displayingProgress: [ :change | change displayingProgressString ].

	tree := self diff: leftTree with: rightTree
]

{ #category : #testing }
IceDiff >> canRevertChanges [
	
	^ source canRevertChanges
]

{ #category : #accessing }
IceDiff >> codeSubdirectoryNode [
	
	^ self repository subdirectory
		ifNotEmpty: [ tree firstNodeSuchThat: [ :definition | self isCodeSubdirectory: definition ] ]
		ifEmpty: [ tree ]
		
]

{ #category : #copying }
IceDiff >> copyWithOnly: aCollection [ 
	
	| treeCopy selectedOperations |
	selectedOperations := aCollection collect: #value.
	treeCopy := tree select: [ :node | selectedOperations includes: node ].
	
	^ self class new
		sourceVersion: source;
		targetVersion: target;
		diffTree: treeCopy;
		yourself
]

{ #category : #accessing }
IceDiff >> description [
	
	^ target description
]

{ #category : #operations }
IceDiff >> diff: leftTree with: rightTree [
	
	| mergedTree |
	mergedTree := leftTree
		mergeWith: rightTree
		onLeft: [ :definition | IceAddition definition: definition ]
		onRight: [ :definition | IceRemoval definition: definition ]
		onMerge: [ :left :right | (left = right)
			ifTrue: [ IceNoModification definition: left ]
			ifFalse: [ IceModification left: left right: right ] ].

	^ mergedTree select: [ :operation | operation hasChanges ]
]

{ #category : #accessing }
IceDiff >> diffTree: anIceTreeDiff [

	tree := anIceTreeDiff
]

{ #category : #building }
IceDiff >> imageChanges [
	^ (source loadedPackages collect: [ :package | IceImageChange package: package ]) asArray
]

{ #category : #testing }
IceDiff >> includesPackageNamed: aString [
	^ self isEmpty not
		and: [ self codeSubdirectoryNode includesKey: aString ]
]

{ #category : #testing }
IceDiff >> isCodeSubdirectory: aNode [
	
	| nodeLocation repositorySubdirectoryLocation |
	nodeLocation := self repository location resolve: aNode path.
	repositorySubdirectoryLocation := self repository subdirectoryReference.
	^ nodeLocation pathString = repositorySubdirectoryLocation pathString
]

{ #category : #testing }
IceDiff >> isEmpty [
	
	^ tree isEmpty
]

{ #category : #testing }
IceDiff >> isValidPackage: aFileReference [
	
	^ self writerClass isValidPackage: aFileReference
]

{ #category : #converting }
IceDiff >> packageNameFromDirectory: aString [ 
	
	^ self writerClass packageNameFromDirectory: aString
]

{ #category : #accessing }
IceDiff >> packages [
	
	tree isEmpty ifTrue: [ ^ #( ) ].
	^ self codeSubdirectoryNode children
]

{ #category : #accessing }
IceDiff >> repository [
	
	^ source repository
]

{ #category : #accessing }
IceDiff >> sourceVersion [
	
	^ source
]

{ #category : #accessing }
IceDiff >> sourceVersion: aCommitish [
	
	source := aCommitish
]

{ #category : #accessing }
IceDiff >> targetVersion [
	
	^ target
]

{ #category : #accessing }
IceDiff >> targetVersion: aCommitish [
	
	target := aCommitish
]

{ #category : #accessing }
IceDiff >> tree [
	^ tree
]

{ #category : #accessing }
IceDiff >> treeToShow [
	"Returns the tree to show in the UI.
	For a diff it is always the same tree.
	Polymorphic with an ice merge"
	^ self tree
]

{ #category : #converting }
IceDiff >> writerClass [
	
	"I am cached per instance of diff because obtaining the writer class is an expensive operation (600ms per call)"
	^ writerClass ifNil: [ writerClass := self repository writerClass ]
]
