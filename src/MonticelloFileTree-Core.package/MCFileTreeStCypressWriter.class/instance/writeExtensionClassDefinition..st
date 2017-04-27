writing
writeExtensionClassDefinition: definition
    | properties |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    STON put: properties asJsonOnStreamPretty: fileStream