visiting
visitMethodDefinition: definition
    | filename directoryname |
    directoryname := definition classIsMeta
        ifTrue: [ definition className , '_class' ]
        ifFalse: [ definition className ].
    filename := self fileNameForSelector: definition selector.
    self
        writeInDirectoryName: directoryname
        fileName: filename
        extension: '.st'
        visit: [ self writeMethodDefinition: definition ]