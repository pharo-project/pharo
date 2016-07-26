initialize-release
writeDefinitions: aCollection
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
