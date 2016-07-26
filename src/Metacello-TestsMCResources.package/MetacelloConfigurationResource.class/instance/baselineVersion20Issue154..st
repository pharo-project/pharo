Issue 154
baselineVersion20Issue154: spec
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'MetacelloExample'
                    with: [ 
                            spec
                                className: 'ConfigurationOfMetacelloExample';
                                versionString: '1.0';
                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
                yourself ]