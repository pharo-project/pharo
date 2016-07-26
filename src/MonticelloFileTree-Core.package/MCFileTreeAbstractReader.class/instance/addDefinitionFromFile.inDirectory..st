utilities
addDefinitionFromFile: directoryEntry inDirectory: aDirectory
    (self definitionFromFile: directoryEntry inDirectory: aDirectory) ifNotNil: [ :def | definitions add: def ]