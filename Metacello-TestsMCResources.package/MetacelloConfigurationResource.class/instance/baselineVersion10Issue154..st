Issue 154
baselineVersion10Issue154: spec
    <version: '1.0-baseline'>
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
                                projectPackage: [ 
                                            spec
                                                name: 'ConfigurationOfMetacelloExample';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]