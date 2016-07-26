initialize-release
writeInitializers
    self
        writeInDirectoryName: self monticelloMetaDirName
        fileName: 'initializers'
        extension: '.st'
        visit: [ 
            stWriter
                writePresentInitializers;
                writeAbsentInitializers ]