writing
writeMethodDefinition: definition
    fileStream
        nextPutAll: definition category;
        lf;
        nextPutAll: definition source withUnixLineEndings