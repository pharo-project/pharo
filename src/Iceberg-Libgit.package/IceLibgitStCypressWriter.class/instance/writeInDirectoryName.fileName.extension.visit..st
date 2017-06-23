visiting
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock
    | directory |
    directory := self directoryForDirectoryNamed: directoryNameOrPath.
    self fileUtils
        writeStreamFor: fileName , ext
        in: directory
        do: [ :fs | 
            self setFileStream: fs.
            visitBlock value ]