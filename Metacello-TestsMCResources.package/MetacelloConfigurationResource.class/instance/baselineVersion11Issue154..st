Issue 154
baselineVersion11Issue154: spec
    <version: '1.1-baseline'>
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
                                                name: 'MetacelloExampleTestConfigurationIssue154';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]