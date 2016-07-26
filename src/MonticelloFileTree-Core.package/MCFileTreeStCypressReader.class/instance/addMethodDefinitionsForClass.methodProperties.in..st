utilities
addMethodDefinitionsForClass: className methodProperties: methodProperties in: entries
  ^ self
    addMethodDefinitionsForClass: className
    methodProperties: methodProperties
    in: entries
    extensionMethod: false