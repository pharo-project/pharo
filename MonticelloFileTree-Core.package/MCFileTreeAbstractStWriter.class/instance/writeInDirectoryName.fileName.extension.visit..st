visiting
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock
    | directory |
    directory := self directoryForDirectoryNamed: directoryNameOrPath.
    self fileUtils
        writeStreamFor: fileName , ext
        in: directory
        do: [ :fileStream | 
            fileStream lineEndConvention: #'lf'.
            self setFileStream: fileStream.
            visitBlock value ]