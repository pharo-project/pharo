utilities
addClassAndMethodDefinitionsFromDirectoryEntries: entries
    | timestamp |
    self noMethodMetaData
        ifTrue: [ timestamp := self info author , ' ' , self info date mmddyyyy , ' ' , self info time print24 ].
    entries
        do: [ :element | 
            element isDirectory
                ifTrue: [ 
                    | directory |
                    directory := self fileUtils directoryFromEntry: element.
                    ((self separateMethodMetaAndSource or: [ self noMethodMetaData ])
                        ifTrue: [ directory entries select: [ :entry | entry name endsWith: '.st' ] ]
                        ifFalse: [ directory entries ])
                        do: [ :file | 
                            | definition |
                            (definition := self definitionFromFile: file inDirectory: directory)
                                ifNotNil: [ 
                                    definition isMethodDefinition
                                        ifTrue: [ 
                                            self separateMethodMetaAndSource
                                                ifTrue: [ 
                                                    directory
                                                        fileNamed: definition selector asString , '.meta'
                                                        do: [ :fileStream | definition setTimeStamp: (Author fixStamp: fileStream contents) ] ].
                                            self noMethodMetaData
                                                ifTrue: [ definition setTimeStamp: timestamp ] ].
                                    definitions add: definition ] ] ] ]