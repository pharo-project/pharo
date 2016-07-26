accessing
projectSpecCreationBlock
    ^ [ :projectName | {(MetacelloMCProject new projectSpec name: projectName)} ]