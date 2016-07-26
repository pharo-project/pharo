writing
writeExtensionClassDefinition: definition
    | properties |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    properties writeCypressJsonOn: fileStream