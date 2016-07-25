writing
writeMethodDefinition: methodDefinition to: methodPath
    | filename |
    filename := self fileNameForSelector: methodDefinition selector.
    self
        writeInDirectoryName: methodPath
        fileName: filename
        extension: '.st'
        visit: [ self writeMethodDefinition: methodDefinition ]