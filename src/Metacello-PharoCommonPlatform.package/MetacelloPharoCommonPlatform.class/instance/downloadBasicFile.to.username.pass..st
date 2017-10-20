git-based http repo support
downloadBasicFile: url to: outputFileName username: username pass: pass
    "download from <url> into <outputFileName>"

    | out err proc errorFileName args |
    "Make a unique name for the error log and ensure that we can write the files"
    errorFileName := self downloadErrorFileNameFor: outputFileName.
    out := FileStream forceNewFileNamed: outputFileName.
    err := FileStream forceNewFileNamed: errorFileName.
    args := username 
      ifNotNil: [ 
        args := {
          '-u'.
          username , ':' , pass. 
          '-L'.
          url} ] 
      ifNil: [
         args := {
          '-L'.
          url}  ].
    proc := (self class environment at: #OSProcess) thisOSProcess
        forkJob: '/usr/bin/curl'
        arguments: args
        environment: nil
        descriptors: (Array with: nil with: out with: err).
     proc ifNil: [ (self class environment at: #OSProcess) noAccessorAvailable ].
     [ proc isRunning ] whileTrue: [ (Delay forMilliseconds: 100) wait ].
     out close.
     err close.
	^ errorFileName 