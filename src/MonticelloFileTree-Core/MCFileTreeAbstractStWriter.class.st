"
Abstract superclass for FileTree writers
"
Class {
	#name : #MCFileTreeAbstractStWriter,
	#superclass : #MCWriter,
	#instVars : [
		'stWriter',
		'initializers',
		'orderedClassNames',
		'orderedTraitNames'
	],
	#category : #'MonticelloFileTree-Core'
}

{ #category : #accessing }
MCFileTreeAbstractStWriter class >> monticelloMetaDirName [
    ^ '.'
]

{ #category : #writing }
MCFileTreeAbstractStWriter class >> on: aStream [
    MCFileTreePackageStructureStWriter useCypressWriter
        ifTrue: [ ^ MCFileTreeStCypressWriter new stream: aStream ].
    ^ (MCFileTreePackageStructureStWriter useSnapShotWriter
        ifTrue: [ MCFileTreeStSnapshotWriter ]
        ifFalse: [ MCFileTreeStWriter ]) new stream: aStream
]

{ #category : #accessing }
MCFileTreeAbstractStWriter class >> readerClass [
	^ MCStReader
]

{ #category : #private }
MCFileTreeAbstractStWriter >> directoryForDirectoryNamed: directoryNameOrPath [
    ^ directoryNameOrPath = '.'
        ifTrue: [ stream packageFileDirectory ]
        ifFalse: [ stream subPackageFileDirectoryFor: directoryNameOrPath ]
]

{ #category : #private }
MCFileTreeAbstractStWriter >> fileNameForSelector: selector [
    ^ (selector == #'/'
        ifTrue: [ 'encoded slash' ]
        ifFalse: [ 
            (selector includes: $/)
                ifTrue: [ 'encoded' , selector copyReplaceAll: '/' with: ' slash ' ]
                ifFalse: [ selector ] ]) asString
]

{ #category : #accessing }
MCFileTreeAbstractStWriter >> fileUtils [
    ^ MCFileTreeFileUtils current
]

{ #category : #initialization }
MCFileTreeAbstractStWriter >> initialize [
    stWriter := MCFileTreePackageStructureStWriter new
        initializers: (initializers := Set new);
        yourself.
    orderedClassNames := OrderedCollection new.
    orderedTraitNames := OrderedCollection new.
]

{ #category : #private }
MCFileTreeAbstractStWriter >> monticelloMetaDirName [
    ^ self class monticelloMetaDirName
]

{ #category : #private }
MCFileTreeAbstractStWriter >> repository [
  ^ stream repository
]

{ #category : #private }
MCFileTreeAbstractStWriter >> setFileStream: file [
    stWriter stream: file
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitClassDefinition: definition [
    self subclassResponsibility
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitClassTraitDefinition: definition [
    self subclassResponsibility
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitMetaclassDefinition: definition [
    "handled by class definition"


]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitMethodDefinition: definition [
    self subclassResponsibility
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitOrganizationDefinition: defintion [
    self
        writeInDirectoryName: self monticelloMetaDirName
        fileName: 'categories'
        extension: '.st'
        visit: [ defintion categories do: [ :cat | stWriter writeCategory: cat ] ]
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitScriptDefinition: definition [
    self flag: #'skippedForNow'
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> visitTraitDefinition: definition [
    self subclassResponsibility
]

{ #category : #'initialize-release' }
MCFileTreeAbstractStWriter >> writeBasicDefinitions: aCollection [
    "the correct initialization order is unknown if some classes are missing in the image"

    self writePropertiesFile.
    stWriter acceptVisitor: self forDefinitions: aCollection.
    self writeInitializers
]

{ #category : #writing }
MCFileTreeAbstractStWriter >> writeClassDefinition: definition [
    stWriter writeClassDefinition: definition.
    (definition hasClassInstanceVariables or: [definition hasClassTraitComposition])
        ifTrue: [ stWriter writeMetaclassDefinition: definition ].
    definition hasComment
        ifTrue: [ stWriter writeClassComment: definition ]
]

{ #category : #writing }
MCFileTreeAbstractStWriter >> writeClassTraitDefinition: definition [
    stWriter visitClassTraitDefinition: definition.

]

{ #category : #initialization }
MCFileTreeAbstractStWriter >> writeDefinitions: aCollection [
    "the correct initialization order is unknown if some classes are missing in the image"

    self writeBasicDefinitions: aCollection
]

{ #category : #visiting }
MCFileTreeAbstractStWriter >> writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock [
    | directory |
    directory := self directoryForDirectoryNamed: directoryNameOrPath.
    self fileUtils
        writeStreamFor: fileName , ext
        in: directory
        do: [ :fileStream |
            self setFileStream: (ZnNewLineWriterStream on: fileStream) forLf.
            visitBlock value ]
]

{ #category : #initialization }
MCFileTreeAbstractStWriter >> writeInitializers [
    self
        writeInDirectoryName: self monticelloMetaDirName
        fileName: 'initializers'
        extension: '.st'
        visit: [ 
            stWriter
                writePresentInitializers;
                writeAbsentInitializers ]
]

{ #category : #writing }
MCFileTreeAbstractStWriter >> writeMethodDefinition: definition [
    stWriter visitMethodDefinition: definition
]

{ #category : #initialization }
MCFileTreeAbstractStWriter >> writePropertiesFile [
    self
        writeInDirectoryName: '.'
        fileName: ''
        extension: '.filetree'
        visit: [ stWriter writeProperties ]
]

{ #category : #writing }
MCFileTreeAbstractStWriter >> writeTraitDefinition: definition [
    stWriter writeClassDefinition: definition.
    definition hasComment 
        ifTrue: [stWriter writeClassComment: definition].
]
