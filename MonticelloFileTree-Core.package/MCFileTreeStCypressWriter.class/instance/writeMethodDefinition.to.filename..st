writing
writeMethodDefinition: methodDefinition to: methodPath filename: filename
  self
    writeInDirectoryName: methodPath
    fileName: filename
    extension: '.st'
    visit: [ self writeMethodDefinition: methodDefinition ]