accessing
projectSpecCreationBlock
    ^ [ :projectName | {(MetacelloMCBaselineProject new baselineOfProjectSpec name: projectName)} ]