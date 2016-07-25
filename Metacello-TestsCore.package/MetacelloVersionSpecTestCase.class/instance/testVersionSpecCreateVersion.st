tests
testVersionSpecCreateVersion
    | spec version |
    spec := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    version := spec createVersion.
    self assert: version class == spec versionClass.
    self assert: version spec == spec.
    self assert: version versionNumber asString = '1.0'.
    self assert: version blessing = #'baseline'.
    self assert: version versionString = '1.0'.
    self assert: version description = 'A description'.
    self assert: version author = 'dkh'.
    self assert: version timestamp = '1/24/2012 09:59'