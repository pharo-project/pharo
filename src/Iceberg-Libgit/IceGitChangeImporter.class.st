Class {
	#name : #IceGitChangeImporter,
	#superclass : #Object,
	#instVars : [
		'path',
		'diff',
		'version'
	],
	#category : #'Iceberg-Libgit-Changes'
}

{ #category : #accessing }
IceGitChangeImporter >> diff: anIceDiff [ 
	diff := anIceDiff
]

{ #category : #building }
IceGitChangeImporter >> ensureDirectory: aFileReference inParentNode: aNode [
	
	aNode childAt: aFileReference basename ifPresent: [ :node |
		self assert: node value isDirectoryDefinition.
		^ node ].
	^ aNode addChild: (IceDirectoryDefinition named: aFileReference basename)
]

{ #category : #building }
IceGitChangeImporter >> ensurePackageFromDirectory: aFileReference inParentNode: aParentNode [
	
	| importer packageName package |
	
	packageName := version writerClass packageNameFromDirectory: aFileReference.
	
	"Package already exists"
	(aParentNode includesKey: packageName)
		ifTrue: [ ^ self ].
	
	package := IcePackage
		named: packageName
		repository: self repository.
	
	importer := IceMCPackageImporter new
		package: package;
		version: version;
		yourself.
	^ importer importOn: aParentNode
]

{ #category : #importing }
IceGitChangeImporter >> importOn: aNode [
	
	| currentSegment fileReference filePath |
	currentSegment := path segments first.
	
	filePath := aNode path / currentSegment.
	fileReference := version fileSystem resolve: filePath.
		
	"Do not import if the file does not exist in git"
	fileReference exists
		ifFalse: [ ^ self ].

	"If I represent a package, let's do it and do nothing else.
	This could be optimized to add the exact methods and avoid MC diff afterwards"
	((diff isCodeSubdirectory: aNode)
		and: [ version writerClass isValidPackage: fileReference ])
			ifTrue: [ ^ self ensurePackageFromDirectory: fileReference inParentNode: aNode ].

	"If I do not represent a package, I may be a file or a directory that may (recursively) contain a method or a package.
	For now, we only support packages inside the declared subdirectory (or root otherwise).
	All other changes are simple directory/file changes"
	
	"If path = 1 this means it is a file"
	path size = 1
		ifTrue: [ 
				fileReference exists ifTrue: [
				^ aNode addChild: (IceFileDefinition named: currentSegment path: filePath fileReference: fileReference) ] ].
	
	path size > 1 ifTrue: [ | directoryReference directoryPath |
		directoryPath := aNode path / currentSegment.
		directoryReference := version fileSystem resolve: directoryPath.
			directoryReference exists ifTrue: [
				"Otherwise, we recursively create a directory structure"
				self nextPath importOn: (self ensureDirectory: fileReference inParentNode: aNode) ] ].
	
	"If path size = 0 we should stop recursion"
]

{ #category : #building }
IceGitChangeImporter >> nextPath [

	^ IceGitChangeImporter new
		path: (RelativePath withAll: path segments allButFirst);
		diff: diff;
		version: version;
		yourself.
]

{ #category : #accessing }
IceGitChangeImporter >> path: aRelativePath [ 
	path := aRelativePath
]

{ #category : #accessing }
IceGitChangeImporter >> repository [
	
	^ diff repository
]

{ #category : #accessing }
IceGitChangeImporter >> version: aVersion [
	
	version := aVersion
]
