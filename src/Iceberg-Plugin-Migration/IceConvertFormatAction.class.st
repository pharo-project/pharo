Class {
	#name : #IceConvertFormatAction,
	#superclass : #Object,
	#instVars : [
		'repository',
		'branchName',
		'format',
		'mergeWithCurrentBranch'
	],
	#category : #'Iceberg-Plugin-Migration'
}

{ #category : #accessing }
IceConvertFormatAction >> branchName [
	^ branchName
]

{ #category : #accessing }
IceConvertFormatAction >> branchName: anObject [
	branchName := anObject
]

{ #category : #accessing }
IceConvertFormatAction >> codeDirectory [
	^ self repository codeDirectory
]

{ #category : #accessing }
IceConvertFormatAction >> codeDirectoryWithDelim [
	 ^ self repository subdirectory isEmptyOrNil 
		ifTrue: [ '' ]
		ifFalse: [ self repository subdirectory, '/' ]
]

{ #category : #private }
IceConvertFormatAction >> ensureMCVersionWithClassOrganisation: mcVersion [
	"if a package does not has class organisation (happens when package is composed 
	 only for extensions, we need to ensure it has one, otherwise package creation will 
	 fail"

	^ mcVersion snapshot definitions 
		detect: [ :each | each isClassDefinition ]
		ifFound: [ :each | mcVersion ]
		ifNone: [ 
			MCVersion 
				package: mcVersion package
				info: mcVersion info
				snapshot: (MCSnapshot fromDefinitions: (
					{ MCOrganizationDefinition categories: { mcVersion package name }}, 
					mcVersion snapshot definitions)) ]
]

{ #category : #execution }
IceConvertFormatAction >> execute [
	| oldBranch newBranch |

	oldBranch := self repository branch.
	self migrateSources.
	newBranch := self repository branch.	
	self isMergeWithCurrentBranch ifTrue: [
		oldBranch checkout: IceCheckoutDoNotLoadPackages new.
		newBranch merge ]
]

{ #category : #accessing }
IceConvertFormatAction >> format [
	^ format
]

{ #category : #accessing }
IceConvertFormatAction >> format: anObject [
	format := anObject
]

{ #category : #testing }
IceConvertFormatAction >> isMergeWithCurrentBranch [
	^ mergeWithCurrentBranch
]

{ #category : #accessing }
IceConvertFormatAction >> mergeWithCurrentBranch: anObject [
	mergeWithCurrentBranch := anObject
]

{ #category : #private }
IceConvertFormatAction >> migratePackage: packageName commit: commit [
	| filetreePackage srcDir subDirWithDelim mcVersion |
	
	srcDir := self codeDirectory.
	subDirWithDelim := self codeDirectoryWithDelim.
	
	mcVersion := (commit versionFor: packageName) mcVersion.
	mcVersion := self ensureMCVersionWithClassOrganisation: mcVersion.
	self writerClass 
		forInternalStoreFileOut: mcVersion 
		on: repository.
	filetreePackage := commit writerClass directoryNameFor: packageName.
	(srcDir / filetreePackage) ensureDeleteAll.
	self repository addFilesToIndex: { 
		subDirWithDelim, (IceLibgitTonelWriter directoryNameFor: packageName).
		subDirWithDelim, (IceLibgitFiletreeWriter directoryNameFor: packageName) }
]

{ #category : #private }
IceConvertFormatAction >> migrateSources [
	| commit |
	
	"branch if you want to perform the migration on separated place (you 
	can later do a PR)"
	self repository createBranch: self branchName.

	commit := self repository branch commit.
	self repository workingCopy packages do: [ :each | 
		self 
			migratePackage: each 
			commit: commit ].

	(self repository workingCopy fileSystem resolve: self repository properties propertiesFilePath)
		ensureDelete;
		binaryWriteStreamDo: [ :stream |
			stream nextPutAll: (self repository properties
				fileFormatId: self format;
				contentsString)  ].
	self repository addFileToIndex: self repository properties propertiesFilePath.
	
	self format = #filetree ifFalse: [ 
		self removeFiletreeRemnants ].
	
	"commit"
	self repository 
		commitIndexWithMessage: 'sources migrated' 
		andParents: { commit }.
]

{ #category : #private }
IceConvertFormatAction >> removeFiletreeRemnants [
	"remove .filetree remaining"
	| filetreeFiles |
	
	filetreeFiles := self codeDirectory allChildrenMatching: '.filetree'.
	filetreeFiles do: #ensureDelete.
	self repository addFilesToIndex: (filetreeFiles collect: [ :each | 
		((each relativeTo: self codeDirectory) printWithDelimiter: $/) ]).
	
]

{ #category : #accessing }
IceConvertFormatAction >> repository [
	^ repository
]

{ #category : #accessing }
IceConvertFormatAction >> repository: anObject [
	repository := anObject
]

{ #category : #accessing }
IceConvertFormatAction >> writerClass [
	
	^ format = #tonel
		ifTrue: [ IceLibgitTonelWriter ]
		ifFalse: [ IceLibgitFiletreeWriter ]
]
