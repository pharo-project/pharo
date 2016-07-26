Issue 154
baselineVersion13Issue154: spec
    <version: '1.3-baseline'>
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
                                                file: 'MetacelloExampleTestConfigurationIssue154.gemstone';
                                                repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ];
                yourself ]