as yet unclassified
writeMethodHolderDefinitions: aCollection extension: extension to: basePath do: aBlock
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
