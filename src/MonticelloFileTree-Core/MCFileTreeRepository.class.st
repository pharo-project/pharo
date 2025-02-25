"
A file tree repository
"
Class {
	#name : 'MCFileTreeRepository',
	#superclass : 'MCDirectoryRepository',
	#instVars : [
		'readonly',
		'repositoryProperties'
	],
	#classInstVars : [
		'defaultPackageExtension',
		'defaultPropertyFileExtension'
	],
	#category : 'MonticelloFileTree-Core',
	#package : 'MonticelloFileTree-Core'
}

{ #category : 'instance creation' }
MCFileTreeRepository class >> basicFromUrl: aZnUrl [
	^ self new directory: (self urlAsFileReference: aZnUrl)
]

{ #category : 'accessing' }
MCFileTreeRepository class >> defaultPackageExtension [
    ".tree, .pkg, .package are the only formats supported at the moment:
	.tree         - original structure
	.pkg          - snapshot structure
	.package - cypress structure"

    defaultPackageExtension
        ifNil: [ 
            defaultPackageExtension := MCFileTreePackageStructureStWriter useCypressWriter
                ifTrue: [ '.package' ]
                ifFalse: [ '.pkg' ] ].
    ^ defaultPackageExtension
]

{ #category : 'accessing' }
MCFileTreeRepository class >> defaultPackageExtension: aString [
    ".tree and .pkg are the only two formats supported at the moment"

    "self defaultPackageExtension:'.package'"

    (#('.tree' '.pkg' '.package') includes: aString)
        ifFalse: [ self error: 'Unsupported package extension: ' , aString printString ].
    defaultPackageExtension := aString
]

{ #category : 'accessing' }
MCFileTreeRepository class >> defaultPropertyFileExtension [
  defaultPropertyFileExtension
    ifNil: [ defaultPropertyFileExtension := '.json' ].
  ^ defaultPropertyFileExtension
]

{ #category : 'accessing' }
MCFileTreeRepository class >> defaultPropertyFileExtension: aString [
  "self defaultPropertyFileExtension:'.ston'"

  self validatePropertyFileExtension: aString.
  defaultPropertyFileExtension := aString
]

{ #category : 'instance creation' }
MCFileTreeRepository class >> description [
    ^ 'filetree://'
]

{ #category : 'utilities' }
MCFileTreeRepository class >> parseName: aString [
    ^ self parseName: aString extension: self defaultPackageExtension
]

{ #category : 'utilities' }
MCFileTreeRepository class >> parseName: aString extension: extension [
    "picked up from GoferVersionReference>>parseName:"

    | basicName package branch author versionNumber packageName |
    basicName := aString last isDigit
        ifTrue: [ aString ]
        ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
    package := basicName copyUpToLast: $-.
    (package includes: $.)
        ifFalse: [ branch := '' ]
        ifTrue: [ 
            branch := '.' , (package copyAfter: $.).
            package := package copyUpTo: $. ].
    author := (basicName copyAfterLast: $-) copyUpToLast: $..
    versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
    (versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
        ifTrue: [ versionNumber := versionNumber asNumber ]
        ifFalse: [ versionNumber := 0 ].
    packageName := package , branch.
    ^ {packageName.
    author.
    versionNumber.
    (packageName , extension)}
]

{ #category : 'utilities' }
MCFileTreeRepository class >> urlAsFileReference: aZnUrl [
	"Extracted from ZnUrl since the scheme is restricted.
	We need to keep host as a segment part."

	| path |
	path := aZnUrl host
		ifNotNil: [ 
			((aZnUrl host = #/) and: [ aZnUrl pathSegments isEmpty ])
				ifTrue: [ ^ FileSystem root ].
			aZnUrl pathSegments copyWithFirst: aZnUrl host ]
		ifNil: [ aZnUrl pathSegments].

	(path ifNotEmpty: [ path first = #'.' or: [ path first = #'..' ] ])
		ifTrue: [ ^ (RelativePath withAll: path) asFileReference ].

	^ (AbsolutePath withAll: path) asFileReference 
]

{ #category : 'accessing' }
MCFileTreeRepository class >> urlSchemes [
	^ #(#filetree)
]

{ #category : 'accessing' }
MCFileTreeRepository class >> validatePropertyFileExtension: aString [
  "see Issue #90: https://github.com/dalehenrich/filetree/issues/90"

  (#('.json' '.ston') includes: aString)
    ifFalse: [ self error: 'Unsupported property file extension: ' , aString printString ]
]

{ #category : 'accessing' }
MCFileTreeRepository >> allFileNames [
    ^ (self directory entries select: [ :entry | entry isDirectory and: [ self canReadFileNamed: entry name ] ])
        collect: [ :entry | entry name ]
]

{ #category : 'accessing' }
MCFileTreeRepository >> allFileNamesForVersionNamed: aString [
	^ self filterFileNames: self readableFileNames forVersionNamed: aString
]

{ #category : 'accessing' }
MCFileTreeRepository >> asRepositorySpecFor: aMetacelloMCProject [
    ^ aMetacelloMCProject repositorySpec
        description: self description;
        type: 'filetree';
        yourself
]

{ #category : 'storing' }
MCFileTreeRepository >> basicStoreVersion: aVersion [
  self readonly
    ifTrue: [ 
      ^ self error: 'The filetree repository: ' , self description printString
            , ' was created read only.' ].
  MCFileTreeWriter fileOut: aVersion on: self
]

{ #category : 'caching' }
MCFileTreeRepository >> cachedFileNames [
	^ #()
]

{ #category : 'testing' }
MCFileTreeRepository >> canReadFileNamed: aString [
    ^ (aString endsWith: self packageExtension)
        or: [ 
            (aString endsWith: '.tree')
                or: [ 
                    "Cypress format"
                    aString endsWith: '.package' ] ]
]

{ #category : 'accessing' }
MCFileTreeRepository >> defaultRepositoryProperties [
  ^ Dictionary new
    at: 'packageExtension' put: self class defaultPackageExtension;
    at: 'propertyFileExtension' put: self propertyFileExtension;
    yourself
]

{ #category : 'descriptions' }
MCFileTreeRepository >> description [
    ^ self class description , super description
]

{ #category : 'accessing' }
MCFileTreeRepository >> directory: aDirectory [
  super directory: aDirectory.
  repositoryProperties := nil.	"force properties to be reloaded from new location"
  self repositoryProperties	"NOW"
]

{ #category : 'actions' }
MCFileTreeRepository >> fileDirectoryOn: directoryPath [
    ^ self fileUtils directoryFromPath: directoryPath relativeTo: self directory
]

{ #category : 'accessing' }
MCFileTreeRepository >> fileUtils [
    ^ MCFileTreeFileUtils current
]

{ #category : 'private' }
MCFileTreeRepository >> flushCache [
  "force properties to be reread ... if the directory exists, otherwise let nature
   take it's course"

  super flushCache.
  directory
    ifNotNil: [ 
      (MCFileTreeFileUtils current directoryExists: directory)
        ifTrue: [ 
          repositoryProperties := nil.
          self repositoryProperties ] ]
]

{ #category : 'accessing' }
MCFileTreeRepository >> goferVersionFrom: aVersionReference [
    "until we no longer find .tree directories in the wild"

    ((self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
        select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ])
        collect: [ :packageDirectory | 
            (self versionInfoForPackageDirectory: packageDirectory) name = aVersionReference name
                ifTrue: [ ^ self loadVersionFromFileNamed: (self fileUtils directoryName: packageDirectory) ] ].
    ^ nil
]

{ #category : 'actions' }
MCFileTreeRepository >> packageDescriptionFromPackageDirectory: packageDirectory [
    | filename info extension |
    filename := self fileUtils current directoryName: packageDirectory.
    extension := filename copyFrom: (filename lastIndexOf: $.) to: filename size.
    ^ ((self packageExtension ~= '.package'
        and: [ 
            (self fileUtils filePathExists: 'version' relativeTo: packageDirectory)
                and: [ self fileUtils filePathExists: 'package' relativeTo: packageDirectory ] ])
        or: [ 
            | dir |
            dir := self fileUtils
                directoryFromPath: MCFileTreeStCypressWriter monticelloMetaDirName
                relativeTo: packageDirectory.
            self fileUtils directoryExists: dir ])
        ifTrue: [ 
            info := self versionInfoForPackageDirectory: packageDirectory.
            self parseName: info name extension: extension ]
        ifFalse: [ 
            {(filename copyFrom: 1 to: (filename lastIndexOf: $.) - 1).
            'cypress'.
            1.
            filename} ]
]

{ #category : 'actions' }
MCFileTreeRepository >> packageDescriptionsFromReadableFileNames [
    ^ ((self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
        select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ])
        collect: [ :packageDirectory | self packageDescriptionFromPackageDirectory: packageDirectory ]
]

{ #category : 'private' }
MCFileTreeRepository >> packageExtension [
  ^ self repositoryProperties
    at: 'packageExtension'
    ifAbsent: [ self class defaultPackageExtension ]
]

{ #category : 'private' }
MCFileTreeRepository >> parseName: aString extension: extension [
    ^ self class parseName: aString extension: extension
]

{ #category : 'private' }
MCFileTreeRepository >> propertyFileExtension [
  ^ self repositoryProperties
    at: 'propertyFileExtension'
    ifAbsent: [ self class defaultPropertyFileExtension ]
]

{ #category : 'private' }
MCFileTreeRepository >> propertyFileExtension: propertyFileExtension [
  self class validatePropertyFileExtension: propertyFileExtension.
  self repositoryProperties
    at: 'propertyFileExtension'
    put: propertyFileExtension.
  self writeRepositoryProperties
]

{ #category : 'i/o' }
MCFileTreeRepository >> readStreamForFileNamed: aString do: aBlock [
    ^ aBlock value: self directory
]

{ #category : 'accessing' }
MCFileTreeRepository >> readonly [
    readonly ifNil: [ readonly := false ].
    ^ readonly
]

{ #category : 'accessing' }
MCFileTreeRepository >> readonly: anObject [
	readonly := anObject
]

{ #category : 'accessing' }
MCFileTreeRepository >> repositoryProperties [

	repositoryProperties
		ifNil: [ repositoryProperties := Dictionary new.
			(self fileUtils directoryExists: directory)
				ifFalse: [ self
						error:
							'filetree:// repository '
								, (self fileUtils directoryPathString: self directory) printString
								, ' does not exist.' ].
			self directory entries
				detect: [ :entry | entry name = '.filetree' ]
				ifFound: [ :configEntry | 
					configEntry
						readStreamDo: [ :fileStream | repositoryProperties := STON fromStream: fileStream ] ]
				ifNone: [ repositoryProperties := self defaultRepositoryProperties.
					self writeRepositoryProperties ] ].
	^ repositoryProperties
]

{ #category : 'accessing' }
MCFileTreeRepository >> versionFromFileNamed: aString [
	^ self loadVersionFromFileNamed: aString
]

{ #category : 'actions' }
MCFileTreeRepository >> versionInfoForPackageDirectory: packageDirectory [
    ^ ((MCReader readerClassForFileNamed: (self fileUtils directoryName: packageDirectory))
        on: (self fileUtils parentDirectoryOf: packageDirectory)
        fileName: (self fileUtils directoryName: packageDirectory))
        loadVersionInfo;
        info
]

{ #category : 'accessing' }
MCFileTreeRepository >> versionInfoFromFileNamed: aString [
	^ self loadVersionInfoFromFileNamed: aString
]

{ #category : 'accessing' }
MCFileTreeRepository >> versionNameFromFileName: aString [
	| description |
	description := self packageDescriptionFromPackageDirectory: (self fileDirectoryOn: aString).
	^ description first , '-' , description second , '.' , description third printString
]

{ #category : 'interface' }
MCFileTreeRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	(self allFileNamesForVersionNamed: aVersionInfo name)
		ifNotEmpty: [ :aCollection | ^ self versionFromFileNamed: aCollection first ].
	^ errorBlock value
]

{ #category : 'testing' }
MCFileTreeRepository >> writeRepositoryProperties [
  self fileUtils
    writeStreamFor: '.filetree'
    in: self directory
    do: [ :fileStream | 
      | keyCount propertyCount |
      repositoryProperties
        ifNil: [ repositoryProperties := self defaultRepositoryProperties ].
      keyCount := repositoryProperties size.
      propertyCount := 0.
      fileStream nextPutAll: '{'.
      repositoryProperties
        keysAndValuesDo: [ :propertyName :propertyValue | 
          propertyCount := propertyCount + 1.
          fileStream
            nextPut: $";
            nextPutAll: propertyName asString;
            nextPutAll: '" : "';
            nextPutAll: propertyValue asString;
            nextPut: $";
            yourself.
          propertyCount < keyCount
            ifTrue: [ 
              fileStream
                nextPutAll: ',';
                lf ] ].
      fileStream nextPutAll: ' }' ]

]

{ #category : 'i/o' }
MCFileTreeRepository >> writeStreamForFileNamed: aString do: aBlock [

	self error: 'we do not open a single stream, but write multiple files'
]
