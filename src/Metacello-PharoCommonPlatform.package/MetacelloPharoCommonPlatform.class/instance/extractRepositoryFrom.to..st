github support
extractRepositoryFrom: zipFile to: directory
    "unzip <zipFile> into <directory>"

    | out err proc errorMessage |
    out := FileStream forceNewFileNamed: '/tmp/zip.out'.
    err := FileStream forceNewFileNamed: '/tmp/zip.err'.
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
        fileNamed: '/tmp/zip.err'
        do: [ :fileStream | 
            (errorMessage := fileStream contentsOfEntireFile) notEmpty
                ifTrue: [ self error: 'unzip failure: ' , errorMessage printString ]
                ifFalse: [
                    (self fileHandleOn: '/tmp')
                        deleteFileNamed: zipFile;
                        deleteFileNamed: (self downloadErrorFileNameFor: zipFile) ] ]
