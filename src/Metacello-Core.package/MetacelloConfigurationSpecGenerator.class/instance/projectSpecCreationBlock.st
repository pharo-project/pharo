accessing
projectSpecCreationBlock
    ^ [ :projectName | {(MetacelloMCProject new configurationOfProjectSpec name: projectName)} ]