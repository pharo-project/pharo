tests
testVersionSpec
    | version |
    version := self versionSpec
        blessing: #'baseline';
        versionString: '1.0';
        description: 'A description';
        author: 'dkh';
        timestamp: '1/24/2012 09:59';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    self assert: version blessing value = #'baseline'.
    self assert: version versionString value = '1.0'.
    self assert: version description value = 'A description'.
    self assert: version author value = 'dkh'.
    self assert: version timestamp value = '1/24/2012 09:59'.
    self assert: version preLoadDoIt value == #'preLoadDoIt'.
    self assert: version postLoadDoIt value == #'postLoadDoIt'.
    self should: [ version preLoadDoIt: '' ] raise: Error.
    self should: [ version postLoadDoIt: '' ] raise: Error