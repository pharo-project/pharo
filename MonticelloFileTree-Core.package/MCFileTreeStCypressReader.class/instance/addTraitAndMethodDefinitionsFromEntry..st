utilities
addTraitAndMethodDefinitionsFromEntry: classEntry
    | classDirectory classPropertiesDict classComment entries methodPropertiesDict |
    classDirectory := self fileUtils directoryFromEntry: classEntry.
    ((entries := classDirectory entries) detect: [:entry | self isPropertyFile: entry] ifNone: [  ])
        ifNotNil: [ :propertyEntry | propertyEntry readStreamDo: [ :fileStream | classPropertiesDict := MCFileTreeJsonParser parseStream: fileStream ] ].
    (entries detect: [ :entry | entry name = 'README.md' ] ifNone: [  ])
        ifNotNil: [ :commentEntry | commentEntry readStreamDo: [ :fileStream | classComment := fileStream contents ] ]
        ifNil: [ classComment := '' ].
    methodPropertiesDict := Dictionary new.
    (entries detect: [ :entry | self isMethodPropertyFile: entry] ifNone: [  ])
        ifNotNil: [ :propertyEntry | 
            propertyEntry
                readStreamDo: [ :fileStream | 
                    "Issue 33: https://github.com/dalehenrich/filetree/issues/33"
                    methodPropertiesDict := MCFileTreeJsonParser parseStream: fileStream ] ].
    self addTraitDefinitionFrom: classPropertiesDict comment: classComment withSqueakLineEndings.
    self
        addMethodDefinitionsForClass: (classPropertiesDict at: 'name')
        methodProperties: methodPropertiesDict
        in: entries