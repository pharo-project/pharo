"
Snapshot writer
"
Class {
	#name : #MCFileTreeStSnapshotWriter,
	#superclass : #MCFileTreeAbstractStWriter,
	#instVars : [
		'classDefinitions',
		'traitDefinitions',
		'classTraitDefinitions',
		'methodDefinitions'
	],
	#category : #'MonticelloFileTree-Core'
}

{ #category : #accessing }
MCFileTreeStSnapshotWriter >> classDefinitions [
    classDefinitions ifNil: [ classDefinitions := Dictionary new ].
    ^ classDefinitions
]

{ #category : #accessing }
MCFileTreeStSnapshotWriter >> classTraitDefinitions [
    classTraitDefinitions ifNil: [ classTraitDefinitions := Dictionary new ].
    ^ classTraitDefinitions
]

{ #category : #accessing }
MCFileTreeStSnapshotWriter >> methodDefinitions [
    methodDefinitions ifNil: [ methodDefinitions := Dictionary new ].
    ^ methodDefinitions
]

{ #category : #accessing }
MCFileTreeStSnapshotWriter >> traitDefinitions [
    traitDefinitions ifNil: [ traitDefinitions := Dictionary new ].
    ^ traitDefinitions
]

{ #category : #visiting }
MCFileTreeStSnapshotWriter >> visitClassDefinition: definition [
    orderedClassNames add: definition className.
    self classDefinitions at: definition className put: definition
]

{ #category : #visiting }
MCFileTreeStSnapshotWriter >> visitClassTraitDefinition: definition [
    orderedTraitNames add: definition className, ' classTrait'.
    self classTraitDefinitions at: definition className put: definition
]

{ #category : #visiting }
MCFileTreeStSnapshotWriter >> visitMethodDefinition: definition [
    (self methodDefinitions
        at: definition className
        ifAbsent: [ self methodDefinitions at: definition className put: OrderedCollection new ]) add: definition
]

{ #category : #visiting }
MCFileTreeStSnapshotWriter >> visitTraitDefinition: definition [
    orderedTraitNames add: definition className.
    self traitDefinitions at: definition className put: definition
]

{ #category : #writing }
MCFileTreeStSnapshotWriter >> writeClassDefinition: definition to: classPath [
    self
        writeInDirectoryName: classPath
        fileName: definition className
        extension: '.st'
        visit: [ self writeClassDefinition: definition ]
]

{ #category : #writing }
MCFileTreeStSnapshotWriter >> writeClassTraitDefinition: definition to: traitPath [
    self
        writeInDirectoryName: traitPath
        fileName: definition className, '_classTrait'
        extension: '.st'
        visit: [ self writeClassTraitDefinition: definition ]
]

{ #category : #initialization }
MCFileTreeStSnapshotWriter >> writeDefinitions: aCollection [
  | basePath extensionClasses extensionMethodDefinitions methodHolders |
  self writeBasicDefinitions: aCollection.
  basePath := 'snapshot' , self fileUtils pathNameDelimiter asString , 'classes'
    , self fileUtils pathNameDelimiter asString.
  extensionClasses := OrderedCollection new.
  extensionMethodDefinitions := OrderedCollection new.
  methodHolders := self classDefinitions, self traitDefinitions.
  self methodDefinitions
    keysAndValuesDo: [ :className :extensionMethods | 
      methodHolders
        at: className
        ifAbsent: [ 
          extensionClasses add: className.
          extensionMethodDefinitions addAll: extensionMethods ] ].
  extensionClasses
    do: [ :className | self methodDefinitions removeKey: className ].
    self writeMethodHolderDefinitions: self traitDefinitions extension: '.trait' to: basePath do: [ :definition :classPath |
	self writeTraitDefinition: definition to: classPath.
	self classTraitDefinitions at: definition className ifPresent: [:classTraitDefinition |
		self writeClassTraitDefinition: classTraitDefinition to: classPath ] ].
  self
    writeMethodHolderDefinitions: self classDefinitions
    extension: '.class'
    to: basePath
    do: [ :definition :classPath | self writeClassDefinition: definition to: classPath ].
  basePath := 'snapshot' , self fileUtils pathNameDelimiter asString
    , 'extensions' , self fileUtils pathNameDelimiter asString.
  extensionMethodDefinitions
    do: [ :methodDefinition | 
      | methodPath |
      methodPath := basePath , methodDefinition className , '.class'
        , self fileUtils pathNameDelimiter asString
        ,
          (methodDefinition classIsMeta
            ifTrue: [ 'class' ]
            ifFalse: [ 'instance' ])
        , self fileUtils pathNameDelimiter asString.
      self writeMethodDefinition: methodDefinition to: methodPath ]

]

{ #category : #writing }
MCFileTreeStSnapshotWriter >> writeMethodDefinition: methodDefinition to: methodPath [
    | filename |
    filename := self fileNameForSelector: methodDefinition selector.
    self
        writeInDirectoryName: methodPath
        fileName: filename
        extension: '.st'
        visit: [ self writeMethodDefinition: methodDefinition ]
]

{ #category : #'as yet unclassified' }
MCFileTreeStSnapshotWriter >> writeMethodHolderDefinitions: aCollection extension: extension to: basePath do: aBlock [
  aCollection
    keysAndValuesDo: [ :className :definition | 
      | classPath instanceMethodPath classMethodPath filenameMetaMap theMethodDefinitions |
      classPath := basePath , definition className , extension
        , self fileUtils pathNameDelimiter asString.
      aBlock value: definition value: classPath.
      instanceMethodPath := classPath , 'instance'
        , self fileUtils pathNameDelimiter asString.
      classMethodPath := classPath , 'class'
        , self fileUtils pathNameDelimiter asString.
      theMethodDefinitions := self methodDefinitions
        at: className
        ifAbsent: [ #() ].
      filenameMetaMap := self fileNameMapFor: theMethodDefinitions.
      theMethodDefinitions
        do: [ :methodDefinition | 
          | filename |
          filename := (filenameMetaMap at: methodDefinition classIsMeta)
            at: methodDefinition selector.
          methodDefinition classIsMeta
            ifTrue: [ 
              self
                writeMethodDefinition: methodDefinition
                to: classMethodPath
                filename: filename ]
            ifFalse: [ 
              self
                writeMethodDefinition: methodDefinition
                to: instanceMethodPath
                filename: filename ] ] ]

]

{ #category : #writing }
MCFileTreeStSnapshotWriter >> writeTraitDefinition: definition to: traitPath [
    self
        writeInDirectoryName: traitPath
        fileName: definition className
        extension: '.st'
        visit: [ self writeTraitDefinition: definition ]
]
