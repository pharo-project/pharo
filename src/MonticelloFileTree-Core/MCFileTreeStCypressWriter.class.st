"
Writer for Cypress format 

see https://github.com/CampSmalltalk/Cypress
"
Class {
	#name : #MCFileTreeStCypressWriter,
	#superclass : #MCFileTreeStSnapshotWriter,
	#instVars : [
		'fileStream'
	],
	#classInstVars : [
		'specials'
	],
	#category : #'MonticelloFileTree-Core'
}

{ #category : #accessing }
MCFileTreeStCypressWriter class >> fileNameForSelector: selector [
  ^ (selector includes: $:)
    ifTrue: [ 
      selector
        collect: [ :each | 
          each = $:
            ifTrue: [ $. ]
            ifFalse: [ each ] ] ]
    ifFalse: [ 
      (self specials includes: selector first)
        ifFalse: [ selector ]
        ifTrue: [ 
          | output specials |
          specials := self specials.
          output := String new writeStream.
          output nextPut: $^.
          selector
            do: [ :each | 
              output
                nextPutAll:
                  ((specials includes: each)
                    ifTrue: [ specials at: each ]
                    ifFalse: [ each asString ]) ]
            separatedBy: [ output nextPut: $. ].
          output contents ] ]

]

{ #category : #initialization }
MCFileTreeStCypressWriter class >> initialize [
    "self initialize"

    "force initialization of specials ..."

    specials := nil
]

{ #category : #private }
MCFileTreeStCypressWriter class >> initializeSpecials [
    | map |
    map := Dictionary new.
    map
        at: $+ put: 'plus';
        at: $- put: 'minus';
        at: $= put: 'equals';
        at: $< put: 'less';
        at: $> put: 'more';
        at: $% put: 'percent';
        at: $& put: 'and';
        at: $| put: 'pipe';
        at: $* put: 'star';
        at: $/ put: 'slash';
        at: $\ put: 'backslash';
        at: $~ put: 'tilde';
        at: $? put: 'wat';
        at: $, put: 'comma';
        at: $@ put: 'at'.
    map keys do: [ :key | map at: (map at: key) put: key ].
    ^ map

]

{ #category : #accessing }
MCFileTreeStCypressWriter class >> monticelloMetaDirName [
    ^ 'monticello.meta'
]

{ #category : #accessing }
MCFileTreeStCypressWriter class >> specials [
    ^ specials ifNil: [ specials := self initializeSpecials ]
]

{ #category : #private }
MCFileTreeStCypressWriter >> fileNameForSelector: selector [
  ^ self class fileNameForSelector: selector

]

{ #category : #private }
MCFileTreeStCypressWriter >> fileNameMapFor: aMethodDefinitionCollection [
  "https://github.com/dalehenrich/filetree/issues/92"

  "answer a dictionary that maps each definition selector to a filename that is guaranteed unique on case insensitive file systems.
  Segregate instance and class side methods. Key is true for class method map, false for instance method map"

  | map filenameMetaMap |
  map := Dictionary new.
  aMethodDefinitionCollection
    do: [ :mDef | 
      | sel col metaKey methMap |
      "sort into bins by lowercase selector. "
      metaKey := mDef classIsMeta.
      methMap := map
        at: metaKey
        ifAbsent: [ map at: metaKey put: Dictionary new ].
      sel := mDef selector asLowercase.
      col := methMap
        at: sel
        ifAbsent: [ methMap at: sel put: OrderedCollection new ].
      col add: mDef ].
  filenameMetaMap := Dictionary new.
  map
    keysAndValuesDo: [ :metaKey :methMap | 
      | filenameMap |
      filenameMap := filenameMetaMap
        at: metaKey
        ifAbsent: [ filenameMetaMap at: metaKey put: Dictionary new ].
      methMap values
        do: [ :col | 
          | selector sortedCol |
          col size = 1
            ifTrue: [ 
              | def |
              "no need to distinguish filename"
              def := col at: 1.
              filenameMap
                at: def selector
                put: (self fileNameForSelector: def selector) ]
            ifFalse: [ 
              "tack on postfix to guarantee  file names are uniique on case insensitive file systems"
              sortedCol := col sorted: [ :a :b | a asString <= b asString ].
              (1 to: sortedCol size) do: [ :index | 
                | def filename |
                def := sortedCol at: index.
                filename := self fileNameForSelector: def selector.
                filename := filename , '..' , index printString.
                filenameMap at: def selector put: filename ] ] ] ].
  ^ filenameMetaMap
]

{ #category : #initialization }
MCFileTreeStCypressWriter >> propertyFileExtension [
  ^ self repository propertyFileExtension
]

{ #category : #private }
MCFileTreeStCypressWriter >> setFileStream: aStream [
    super setFileStream: aStream.
    fileStream := aStream
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeClassComment: definition [
    fileStream nextPutAll: definition comment withUnixLineEndings
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeClassDefinition: definition [
    | properties |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    properties at: 'super' put: definition superclassName.
    definition traitCompositionString
        ifNotNil: [ :property | 
            "Issue #48: https://github.com/dalehenrich/filetree/issues/48"
            property ~= '{}'
                ifTrue: [ properties at: 'traitcomposition' put: property ] ].
    definition classTraitCompositionString
        ifNotNil: [ :property | 
            "Issue #48: https://github.com/dalehenrich/filetree/issues/48"
            property ~= '{}'
                ifTrue: [ properties at: 'classtraitcomposition' put: property ] ].
    properties at: 'category' put: definition category.
    properties at: 'instvars' put: definition instVarNames asArray.
    properties at: 'classvars' put: definition classVarNames asArray.
    properties at: 'pools' put: definition poolDictionaries asArray.
    properties at: 'classinstvars' put: definition classInstVarNames asArray.
    properties at: 'type' put: definition type asString.
    properties at: 'commentStamp' put: definition commentStamp.
    STON put: properties asJsonOnStreamPretty: fileStream
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeClassDefinition: definition to: classPath [
  self
    writeInDirectoryName: classPath
    fileName: 'README'
    extension: '.md'
    visit: [ self writeClassComment: definition ].
  self
    writeInDirectoryName: classPath
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ self writeClassDefinition: definition ].
  self
    writeInDirectoryName: classPath
    fileName: 'methodProperties'
    extension: self propertyFileExtension
    visit: [ 
      self
        writeMethodProperties:
          (self methodDefinitions at: definition className ifAbsent: [ #() ]) ]
]

{ #category : #initialization }
MCFileTreeStCypressWriter >> writeDefinitions: aCollection [
  | classDirExtension extensionClasses extensionMethodDefinitions extensionMethodMap methodHolders |
  self writeBasicDefinitions: aCollection.
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
    self writeMethodHolderDefinitions: self traitDefinitions extension: '.trait' to: '' do: [ :definition :classPath |
	self writeTraitDefinition: definition to: classPath. ].
  self
    writeMethodHolderDefinitions: self classDefinitions
    extension: '.class'
    to: ''
    do: [ :definition :classPath | self writeClassDefinition: definition to: classPath ].
  classDirExtension := '.extension'.
  extensionMethodMap := Dictionary new.
  extensionMethodDefinitions
    do: [ :methodDefinition | 
      | classPath methodPath |
      (extensionMethodMap
        at: methodDefinition className
        ifAbsent: [ extensionMethodMap at: methodDefinition className put: OrderedCollection new ])
        add: methodDefinition.
      classPath := methodDefinition className , classDirExtension
        , self fileUtils pathNameDelimiter asString.
      self writeExtensionClassDefinition: methodDefinition to: classPath ].
  extensionMethodMap
    keysAndValuesDo: [ :className :classMethodDefinitions | 
      | classPath filenameMetaMap |
      filenameMetaMap := self fileNameMapFor: classMethodDefinitions.
      classMethodDefinitions
        do: [ :methodDefinition | 
          | filename methodPath |
          filename := (filenameMetaMap at: methodDefinition classIsMeta)
            at: methodDefinition selector.
          classPath := methodDefinition className , classDirExtension
            , self fileUtils pathNameDelimiter asString.
          methodPath := classPath
            ,
              (methodDefinition classIsMeta
                ifTrue: [ 'class' ]
                ifFalse: [ 'instance' ])
            , self fileUtils pathNameDelimiter asString.
          self
            writeMethodDefinition: methodDefinition
            to: methodPath
            filename: filename ].
      classPath := className , classDirExtension
        , self fileUtils pathNameDelimiter asString.
      self
        writeInDirectoryName: classPath
        fileName: 'methodProperties'
        extension: self propertyFileExtension
        visit: [ self writeMethodProperties: classMethodDefinitions ] ]

]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeExtensionClassDefinition: definition [
    | properties |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    STON put: properties asJsonOnStreamPretty: fileStream
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeExtensionClassDefinition: definition to: classPath [
  self
    writeInDirectoryName: classPath
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ self writeExtensionClassDefinition: definition ]
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeMethodDefinition: definition [
    fileStream
        nextPutAll: definition category;
        lf;
        nextPutAll: definition source withUnixLineEndings
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeMethodDefinition: methodDefinition to: methodPath [
  self shouldNotImplement
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeMethodDefinition: methodDefinition to: methodPath filename: filename [
  self
    writeInDirectoryName: methodPath
    fileName: filename
    extension: '.st'
    visit: [ self writeMethodDefinition: methodDefinition ]
]

{ #category : #'initialize-release' }
MCFileTreeStCypressWriter >> writeMethodProperties: classMethodDefinitions [
    "Issue 33: https://github.com/dalehenrich/filetree/issues/33"

    | properties classMethodsMap instanceMethodMap |
    properties := Dictionary new.
    properties at: 'class' put: (classMethodsMap := Dictionary new).
    properties at: 'instance' put: (instanceMethodMap := Dictionary new).
    classMethodDefinitions
        do: [ :methodDefinition | 
            (methodDefinition classIsMeta
                ifTrue: [ classMethodsMap ]
                ifFalse: [ instanceMethodMap ]) at: methodDefinition selector asString put: methodDefinition timeStamp ].
    STON put: properties asJsonOnStreamPretty: fileStream
]

{ #category : #initialization }
MCFileTreeStCypressWriter >> writePropertiesFile [
  | properties |
  properties := Dictionary new.
  properties at: 'noMethodMetaData' put: true.
  properties at: 'separateMethodMetaAndSource' put: false.
  properties at: 'useCypressPropertiesFile' put: true.
  self
    writeInDirectoryName: '.'
    fileName: ''
    extension: '.filetree'
    visit: [ STON put: properties asJsonOnStreamPretty: fileStream ].
  self
    writeInDirectoryName: '.'
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ STON put: (Dictionary new) asJsonOnStreamPretty: fileStream ]
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeTraitDefinition: definition [
    | properties compositionString |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    definition traitCompositionString
        ifNotNil: [ :property | 
            property ~= '{}'
                ifTrue: [ properties at: 'traitcomposition' put: property ] ].
    " handle the classTrait case "
    compositionString := self classTraitDefinitions at: definition className ifPresent: [:classTraitDefinition |
	classTraitDefinition classTraitCompositionString ].
    compositionString ifNil: [ compositionString := definition classTraitCompositionString ].
    compositionString~= '{}'
                ifTrue: [ properties at: 'classtraitcomposition' put: compositionString ] .
    properties at: 'category' put: definition category.
    properties at: 'commentStamp' put: definition commentStamp.
    properties at: 'instvars' put: definition instVarNames asArray.
    properties at: 'classinstvars' put: definition classInstVarNames asArray.
    STON put: properties asJsonOnStreamPretty: fileStream
]

{ #category : #writing }
MCFileTreeStCypressWriter >> writeTraitDefinition: definition to: classPath [
  self
    writeInDirectoryName: classPath
    fileName: 'README'
    extension: '.md'
    visit: [ self writeClassComment: definition ].
  self
    writeInDirectoryName: classPath
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ self writeTraitDefinition: definition ].
  self
    writeInDirectoryName: classPath
    fileName: 'methodProperties'
    extension: self propertyFileExtension
    visit: [ 
      self
        writeMethodProperties:
          (self methodDefinitions at: definition className ifAbsent: [ #() ]) ]
]
