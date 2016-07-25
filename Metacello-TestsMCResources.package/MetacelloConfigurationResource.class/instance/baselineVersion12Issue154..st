Issue 154
baselineVersion12Issue154: spec
    <version: '1.2-baseline'>
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
                                                file: 'MetacelloExampleTestConfigurationIssue154.gemstone';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]