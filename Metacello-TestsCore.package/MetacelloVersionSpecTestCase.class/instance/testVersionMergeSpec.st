tests
testVersionMergeSpec
    | versionA versionB version |
    versionA := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    versionB := self versionSpec
        blessing: #'release';
        versionString: '1.1';
        description: 'A FULL description';
        author: 'DaleHenrichs';
        timestamp: '1/24/2012 10:22';
        preLoadDoIt: #'preLoadDoItB';
        postLoadDoIt: #'postLoadDoItB';
        yourself.
    version := versionA mergeSpec: versionB.
    self assert: version blessing value = #'release'.
    self assert: version versionString value = '1.1'.
    self assert: version description value = 'A FULL description'.
    self assert: version author value = 'DaleHenrichs'.
    self assert: version timestamp value = '1/24/2012 10:22'.
    self assert: version preLoadDoIt value == #'preLoadDoItB'.
    self assert: version postLoadDoIt value == #'postLoadDoItB'