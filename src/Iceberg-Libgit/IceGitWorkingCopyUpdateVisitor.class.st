Class {
	#name : #IceGitWorkingCopyUpdateVisitor,
	#superclass : #IceTreeVisitor,
	#instVars : [
		'repository',
		'diff',
		'index'
	],
	#category : #'Iceberg-Libgit-Commit'
}

{ #category : #accessing }
IceGitWorkingCopyUpdateVisitor >> diff: anIceDiff [ 
	diff := anIceDiff
]

{ #category : #accessing }
IceGitWorkingCopyUpdateVisitor >> index: anIndex [ 
	index := anIndex
]

{ #category : #operations }
IceGitWorkingCopyUpdateVisitor >> removeFile: anIceFileDefinition [ 
	
	(repository location resolvePath: anIceFileDefinition path)
		ensureDelete
]

{ #category : #operations }
IceGitWorkingCopyUpdateVisitor >> removePackage: aDefinition [
	
	(repository location resolvePath: aDefinition path) ensureDeleteAll
]

{ #category : #accessing }
IceGitWorkingCopyUpdateVisitor >> repository: anIceLibgitRepository [ 
	repository := anIceLibgitRepository
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitAddition: anIceAddition [ 
	
	anIceAddition definition accept: self
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitDirectoryDefinition: anIceDirectoryDefinition [ 
	self visitChildrenOf: currentNode
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitFileNode: anIceFileDefinition [ 
	
	| reference |
	reference := repository location resolve: anIceFileDefinition path.
	reference parent ensureCreateDirectory.
	reference
		ensureDelete;
		binaryWriteStreamDo: [ :writeStream |
			writeStream nextPutAll: anIceFileDefinition basicContents ].
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitModification: anIceModification [ 
	
	^ anIceModification leftDefinition accept: self
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitNoModification: anIceNoModification [ 
	
	anIceNoModification definition accept: self
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitNonConflictingOperation: anIceNonConflictingOperation [ 
	
	anIceNonConflictingOperation chosenOperation accept: self
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitPackage: anIcePackageDefinition [ 
	
	"I am a package, I have to export my changes using the repository format.
	Thus, I cut here the recursion on my children, I have to do the recursion myself"
	| mcPackage snapshot patcher version |
	mcPackage := MCPackage named: anIcePackageDefinition name.

	"Take a snapshot of the package from the version we want to apply our changes on (usually the commit in disk) and apply them to it.
	This will create a snapshot with only the selected changes"
	snapshot := [diff targetVersion snapshotFor: anIcePackageDefinition]
		on: NotFound do: [ MCSnapshot empty ].
	patcher := MCPatcher snapshot: snapshot.
	currentNode accept: (IceMCSnapshotPatchVisitor new
		patcher: patcher;
		yourself).

	version := MCVersion new
		setPackage: mcPackage
		info: (IceMCVersionInfo package: mcPackage message: 'Internal...')
		snapshot: patcher patchedSnapshot
		dependencies: #().

	"Save version to disk.
	The repository will take care of the correct output format.
	Note: this step does not perform a commit, it just exports changes so the repository can then perform a commit"
	
	index storeVersion: version
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitRemoval: anIceRemoval [ 
	
	anIceRemoval definition removeFrom: self
]

{ #category : #visiting }
IceGitWorkingCopyUpdateVisitor >> visitRootDefinition: anIceRootDefinition [ 
	
	self visitChildrenOf: currentNode
]
