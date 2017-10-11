github support
extractRepositoryFrom: zipFile to: directory
    "unzip <zipFile> into <directory>"

    | out err proc errorMessage errorFileName |
    errorFileName := self downloadErrorFileNameFor: zipFile.
    out := FileStream forceNewFileNamed: '/tmp/zip.out'.
    err := FileStream forceNewFileNamed: errorFileName.
    errorMessage := ''.
    [ 
    proc := (self class environment at: #OSProcess) thisOSProcess
        forkJob: '/usr/bin/unzip'
        arguments:
            {'-u'.
            zipFile.
            '-d'.
            directory}
        environment: nil
        descriptors: (Array with: nil with: out with: err).
    proc ifNil: [ self noAccessorAvailable ].
    [ proc isRunning ] whileTrue: [ (Delay forMilliseconds: 100) wait ] ]
        ensure: [ 
            out close.
            err close ].
    FileStream
        fileNamed: errorFileName
        do: [ :fileStream | 
            (errorMessage := fileStream contentsOfEntireFile) notEmpty
                ifTrue: [ self error: 'unzip failure: ' , errorMessage printString ]
                ifFalse: [
                    (self fileHandleOn: '/tmp')
                        deleteFileNamed: zipFile;
                        deleteFileNamed: errorFileName ] ]
