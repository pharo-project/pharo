Class {
	#name : #MetacelloScriptingResource,
	#superclass : #TestResource,
	#instVars : [
		'configurationRepository',
		'externalRepository',
		'sampleRepository'
	],
	#category : #'Metacello-TestsMCResources'
}

{ #category : #accessing }
MetacelloScriptingResource class >> externalConfigurationSHA [
    "SHA of commit on 'configuration' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

    "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

    "self reset"

    ^ 'd91949731736bf48879781c29c7365feca461cde'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue181SHA [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '36ac04abfc3a173de9432a134a718bf14b0a6968'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue185SHA [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '29efb3d9cb86da55402a543cbef5771e39f864e7'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue185SHA1 [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '96cf2ed0a252ce55b7fd5ec4bedf3ad999ca6d23'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue185SHA2 [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '9e79d0e165bd51f7476d10a07968706f6034a6e6'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue185SHA3 [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '11dfa72bc4a956ce0cb5a7603ea39f5abf73ff1c'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue185SHA4 [
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ 'e36dcd023518f4d2a209a482097f5a8ef745ef25'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue234ShortMasterSHA [
  "commit on branch issue/234 of External project: 
   master branch
   78b9c1d134fbf76001cc0b1a10e3b1a0c13eec9d"

  ^ '78b9c1d'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomIssue234ShortSHA [
  "commit on branch issue/234 of External project: 
   issue/234 branch
x   issue\234 branch
x   issue\\234 branch
x   issue\xxx\branch branch
   issue/xxx/branch branch
x   issue//234 branch
   b57801b1f9386e37c1675cd9f9e04b0c66a3f60b"

  ^ 'b57801b'
]

{ #category : #accessing }
MetacelloScriptingResource class >> externalCustomSHA [
    "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

    "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

    "self reset"

    ^ '1ac58502ade7814e1590f71d615cca434b1a4fd5'
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> baselineGithubReferenceIV: spec [
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>baselineGithubReferenceIV:'.
      spec
        baseline: 'External'
        with: [ spec repository: 'http://ss3.gemtalksystems.com/ss/external' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> baselineGithubReferenceV: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceV:'.
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> baselineGithubReferenceVI: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceVI:'.
            spec
                baseline: 'External Core'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ];
                baseline: 'External Tests'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Tests';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> baselineGithubReferenceVII: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceVII:'.
            spec
                baseline: 'External Core'
                    with: [ 
                            spec
                                className: 'BaselineOfExternal';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ];
                project: 'External Tests' copyFrom: 'External Core' with: [ spec loads: 'Tests' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> baselineGithubReferenceXX: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineGithubReferenceXX:'.
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]
]

{ #category : #'issue 215' }
MetacelloScriptingResource >> baselineIssue215: spec [
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec description: self className , '>>baselineGithubReferenceV:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        import: 'External' provides: #('External-Core' 'External-Tests') ]
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> baselineIssue32: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>baselineIssue32:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                preLoadDoIt: #'preloadDoIt';
                postLoadDoIt: #'postloadDoIt';
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ]
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> baselineIssue399: spec [
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec description: self className , '>>baselineIssue399:'.
      spec package: 'Issue399-Core' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration091Issue181: spec [
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration091Issue181:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration091Issue63: spec [
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration091Issue63:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration092Issue181: spec [
  <version: '0.9.2'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration092Issue181:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue181SHA
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration092Issue63: spec [
  <version: '0.9.2'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration092Issue63:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        package: 'GoferBar'
          with: [ 
              spec
                file: 'GoferBar-lr.1';
                repository: 'dictionary://Metacello_Gofer_Test_Repository' ];
        package: 'GoferFoo'
          with: [ 
              spec
                file: 'GoferFoo-lr.2';
                requires: 'External';
                repository: 'dictionary://Metacello_Gofer_Test_Repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration093Issue185: spec [
  <version: '0.9.3'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration093Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration094Issue185: spec [
  <version: '0.9.4'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration094Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA1
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration095Issue185: spec [
  <version: '0.9.5'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration095Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA2
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration096Issue185: spec [
  <version: '0.9.6'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration0956Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA3
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> configuration097Issue185: spec [
  <version: '0.9.7'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configuration0957Issue185:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:'
                    , MetacelloScriptingResource externalCustomIssue185SHA4
                    , '/repository' ];
        import: 'External' ]
]

{ #category : #'issue 215' }
MetacelloScriptingResource >> configuration098Issue215: spec [
  <version: '0.9.8'>
  spec
    for: #'common'
    do: [ 
      spec description: self className , '>>configuration092Issue63:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        package: 'GoferBar'
          with: [ 
              spec
                file: 'GoferBar-lr.1';
                requires: 'External-Core';
                repository: 'dictionary://Metacello_Gofer_Test_Repository' ];
        package: 'GoferFoo'
          with: [ 
              spec
                file: 'GoferFoo-lr.2';
                requires: 'External-Tests';
                repository: 'dictionary://Metacello_Gofer_Test_Repository' ];
        import: 'External' provides: #('External-Core' 'External-Tests') ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationExternalRef090: spec [
  <version: '0.9.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'version'.
      spec
        description: 'MetacelloScriptingResource>>configurationExternalRef090:'.
      spec
        project: 'External'
        with: [ 
          spec
            className: 'ConfigurationOfExternal';
            version: '0.9.0';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationExternalRef091: spec [
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'version'.
      spec
        description: 'MetacelloScriptingResource>>configurationExternalRef090:'.
      spec
        project: 'External'
        with: [ 
          spec
            className: 'ConfigurationOfExternal';
            version: '0.9.1';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationExternalRef092: spec [
  <version: '0.9.2'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'version'.
      spec
        description: 'MetacelloScriptingResource>>configurationExternalRef092:'.
      spec
        project: 'External'
        with: [ 
          "ConfigurationOfExternal 0.9.2 should not exist"
          spec
            className: 'ConfigurationOfExternal';
            version: '0.9.2';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationGithubReferenceIV: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceIV:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationGithubReferenceV: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceV:'.
            spec
                project: 'External'
                    with: [ 
                            spec
                                className: 'ConfigurationOfExternal';
                                version: '0.9.0';
                                loads: 'Core';
                                repository:
                                        'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ];
                project: 'External Tests' copyFrom: 'External' with: [ spec loads: 'Tests' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationGithubReferenceX: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceX:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationGithubReferenceXX: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>configurationGithubReferenceXX:'.
            spec
                configuration: 'External'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> configurationGithubReferenceXXX: spec [
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>configurationGithubReferenceXXX:'.
      spec
        configuration: 'External'
        with: [ 
          spec
            version: '0.9.0';
            repository: 'http://ss3.gemtalksystems.com/ss/external' ] ]
]

{ #category : #accessing }
MetacelloScriptingResource >> configurationRepository [
    ^ configurationRepository
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> conflictOf10: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>conflictOf10:'.
            spec author: 'dkh'.
            spec timestamp: '6/1/2012 14:46' ].
    spec
        for: #'custom'
        do: [ 
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        version: '0.9.1';
                        repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> conflictOf11: spec [
    <version: '1.1.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>conflictOf11:'.
            spec author: 'dkh'.
            spec timestamp: '6/1/2012 14:46' ].
    spec
        for: #'custom'
        do: [ 
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> conflictOf12: spec [
    <version: '1.2.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>conflictOf12:'.
            spec author: 'dkh'.
            spec timestamp: '6/1/2012 14:46' ].
    spec
        for: #'custom'
        do: [ 
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        operator: #'=';
                        version: '0.9.2';
                        repository: 'dictionary://Metacello_Conflict_Test_Repository'	"#= forces conflict" ] ]
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> conflictOf20: spec [
  <version: '2.0.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>conflictOf20:'.
      spec author: 'dkh'.
      spec timestamp: '6/1/2012 14:46' ].
  spec
    for: #'custom'
    do: [ 
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: '0.9.0';
            repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> conflictOf21: spec [
  <version: '2.1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>conflictOf21:'.
      spec author: 'dkh'.
      spec timestamp: '6/1/2012 14:46' ].
  spec
    for: #'custom'
    do: [ 
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            operator: #'=';
            version: '0.9.1';
            repository: 'dictionary://Metacello_Conflict_Test_Repository' ] ]
]

{ #category : #'external repository' }
MetacelloScriptingResource >> customProjectAttributes [
    ^ #(#custom)
]

{ #category : #'external core' }
MetacelloScriptingResource >> externalAuthorName [
    ^ 'dkh'
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> externalBaselineX: spec [
    <baseline>
    spec description: 'MetacelloScriptingResource>>externalBaselineX:'.
    spec
        package: 'External-CoreX';
        package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
        yourself.
    spec
        group: 'Core' with: #('External-CoreX');
        group: 'default' with: #('Core');
        group: 'Tests' with: #('External-TestsX');
        yourself
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> externalBaselineXX: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>externalBaselineXX:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ]
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> externalBaselineXXX: spec [
    <baseline>
    spec
        for: #'common'
        do: [ 
            spec description: 'MetacelloScriptingResource>>externalBaselineXXX:'.
            spec
                package: 'External-CoreX';
                package: 'External-TestsX' with: [ spec requires: 'External-CoreX' ];
                yourself.
            spec
                group: 'Core' with: #('External-CoreX');
                group: 'default' with: #('Core');
                group: 'Tests' with: #('External-TestsX');
                yourself ].
    spec
        for: #'custom'
        do: [ 
            spec package: 'External-UIX'.
            spec group: 'UI' with: #('External-UIX') ]
]

{ #category : #accessing }
MetacelloScriptingResource >> externalRepository [
    ^ externalRepository
]

{ #category : #'invalid configurations' }
MetacelloScriptingResource >> invalidConfiguration10: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration10: ... missing className:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]
]

{ #category : #'invalid configurations' }
MetacelloScriptingResource >> invalidConfiguration20: spec [
    <version: '2.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration20: ... missing version:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        repository:
                                'github://dalehenrich/external:' , MetacelloScriptingResource externalConfigurationSHA , '/repository' ] ]
]

{ #category : #'invalid configurations' }
MetacelloScriptingResource >> invalidConfiguration30: spec [
    <version: '3.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>invalidConfiguration30: ... missing repository:'.
            spec
                project: 'External'
                with: [ 
                    spec
                        className: 'ConfigurationOfExternal';
                        version: '0.9.0' ] ]
]

{ #category : #'external core' }
MetacelloScriptingResource >> isExternal [
    ^ false
]

{ #category : #'sample repository' }
MetacelloScriptingResource >> isSample [
    ^ false
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration10: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>lockConfiguration10: '.
            spec
                configuration: 'ExternalX'
                with: [ 
                    spec
                        version: '0.9.0';
                        repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration11: spec [
  <version: '1.1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration11: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: '0.9.1';
            repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration12: spec [
  <version: '1.2.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration12: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          "bleedingEdge version"
          spec repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration13: spec [
  <version: '1.3.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration13: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: #'stable';
            repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration14: spec [
  <version: '1.4.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: 'MetacelloScriptingResource>>lockConfiguration14: '.
      spec
        configuration: 'ExternalX'
        with: [ 
          spec
            version: #'unstable';
            repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #locking }
MetacelloScriptingResource >> lockConfiguration15: spec [
  <version: '1.5.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec description: self className , '>>lockConfiguration15: '.
      spec
        baseline: 'External'
        with: [ 
          spec
            repository:
              'github://dalehenrich/external:'
                , MetacelloScriptingResource externalCustomIssue185SHA
                , '/repository' ] ]
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> postloadDoIt [
	"self reset"

	Smalltalk at: #'Metacello_Configuration_Test_POST_DoIt_Result' put: true
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> preloadDoIt [
	"self reset"

	Smalltalk at: #'Metacello_Configuration_Test_DoIt_Result' put: true
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> projectClass [
  "Issue399 uses this to test Cypress package loading"

  ^ Smalltalk at: #'MetacelloCypressBaselineProject'
]

{ #category : #'sample repository' }
MetacelloScriptingResource >> sampleAuthorName [
    ^ (Smalltalk at: #'ExternalCore') new externalAuthorName
]

{ #category : #accessing }
MetacelloScriptingResource >> sampleRepository [
    ^ sampleRepository
]

{ #category : #running }
MetacelloScriptingResource >> setUp [
  "self reset"

  super setUp.
  self
    setUpRepositories;
    setUpExternalCore;
    setUpExternalCoreX;
    setUpSampleCore;
    setUpBaselineOfExternalX;
    setUpBaselineOfExternalXX;
    setUpBaselineOfExternalXXX;
    setUpConfigurationOfExternaldkh1;
    setUpConfigurationOfExternaldkh2;
    setUpConfigurationExternalRef;
    setUpConfigurationOfExternalX;
    setUpConfigurationOfExternalXX;
    setUpConfigurationOfExternalXXX;
    setUpConfigurationOfExternalIV;
    setUpBaselineGithubReferenceXX;
    setUpBaselineGithubReferenceIV;
    setUpBaselineGithubReferenceV;
    setUpBaselineGithubReferenceVI;
    setUpBaselineGithubReferenceVII;
    setUpBaselineGithubReferenceVIII;
    setUpBaselineGithubReferenceIX;
    setUpBaselineGithubReferenceXI;
    setUpBaselineGithubReferenceXII;
    setUpBaselineGithubReferenceXIII;
    setUpMarianosImage;
    setUpConfigurationOfConflict;
    setUpInvalidConfigurations;
    setUpConfigurationIssue32;
    setUpBaselineIssue32;
    setUpConfigurationIssue59;
    setUpConfiguration63;
    setUpLockConfigurations;
    setUpConfigurationIssue84;
    setUpConfigurationNextedIssue84;
    setUpConfiguration181;
    setUpBaselineIssue215;
    setUpConfigurationIssue339;
    setUpIssue399
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceIV [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefIV-dkh.1'.
    className := #'BaselineOfGithubRefIV'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configurationGithubReferenceX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceIX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefIX-dkh.1'.
    className := #'BaselineOfGithubRefIX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configurationGithubReferenceIV:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceIV:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceV [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefV-dkh.1'.
    className := #'BaselineOfGithubRefV'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configurationGithubReferenceXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceXX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceVI [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefVI-dkh.1'.
    className := #'BaselineOfGithubRefVI'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configurationGithubReferenceXXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceXXX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceVII [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefVII-dkh.1'.
    className := #'BaselineOfGithubRefVII'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineGithubReferenceIV:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineGithubReferenceIV:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceVIII [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefVIII-dkh.1'.
    className := #'BaselineOfGithubRefVIII'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineGithubReferenceV:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineGithubReferenceV:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceXI [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefXI-dkh.1'.
    className := #'BaselineOfGithubRefXI'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configurationGithubReferenceV:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configurationGithubReferenceV:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceXII [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefXII-dkh.1'.
    className := #'BaselineOfGithubRefXII'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineGithubReferenceVI:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineGithubReferenceVI:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceXIII [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefXIII-dkh.1'.
    className := #'BaselineOfGithubRefXIII'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineGithubReferenceVII:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineGithubReferenceVII:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpBaselineGithubReferenceXX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfGithubRefXX-dkh.1'.
    className := #'BaselineOfGithubRefXX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineGithubReferenceXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineGithubReferenceXX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 215' }
MetacelloScriptingResource >> setUpBaselineIssue215 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'BaselineOfIssue215-dkh.1'.
  className := #'BaselineOfIssue215'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'BaselineOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'postloadDoIt'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'postloadDoIt') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'preloadDoIt'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'preloadDoIt') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'baselineIssue215:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineIssue215:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'customProjectAttributes'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> setUpBaselineIssue32 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfIssue32-dkh.1'.
    className := #'BaselineOfIssue32'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'postloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'postloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'preloadDoIt'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'preloadDoIt') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'baselineIssue32:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'baselineIssue32:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpBaselineIssue399 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'BaselineOfIssue399-dkh.1'.
  className := #'BaselineOfIssue399'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'BaselineOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'baselineIssue399:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineIssue399:') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpBaselineIssue399Cypress [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'BaselineOfIssue399Cypress-dkh.1'.
  className := #'BaselineOfIssue399Cypress'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'BaselineOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'baselineIssue399:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'baselineIssue399:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'projectClass'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'projectClass') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> setUpBaselineOfExternalX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfExternalX-dkh.1'.
    className := #'BaselineOfExternalX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'externalBaselineX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'externalBaselineX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> setUpBaselineOfExternalXX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfExternalXX-dkh.1'.
    className := #'BaselineOfExternalXX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'externalBaselineXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'externalBaselineXX:') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - external' }
MetacelloScriptingResource >> setUpBaselineOfExternalXXX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'BaselineOfExternalXXX-dkh.1'.
    className := #'BaselineOfExternalXXX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'BaselineOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'externalBaselineXXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'externalBaselineXXX:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    externalRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> setUpConfiguration181 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfIssue181-dkh.1'.
  className := #'ConfigurationOfIssue181'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration091Issue181:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration091Issue181:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration092Issue181:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration092Issue181:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration093Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration093Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration094Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration094Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration095Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration095Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration096Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration096Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration097Issue185:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration097Issue185:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configuration098Issue215:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configuration098Issue215:') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 63' }
MetacelloScriptingResource >> setUpConfiguration63 [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue63-dkh.1'.
    className := #'ConfigurationOfIssue63'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configuration091Issue63:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configuration091Issue63:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'configuration092Issue63:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'configuration092Issue63:') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpConfigurationExternalRef [
  "self reset"

  | versionInfo |
  versionInfo := self setUpConfigurationExternalRefdkh1.
  versionInfo := self setUpConfigurationExternalRefdkh2: {versionInfo}
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpConfigurationExternalRefdkh1 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalRef-dkh.1'.
  className := #'ConfigurationOfExternalRef'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef091:') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'baseline pragma - github reference' }
MetacelloScriptingResource >> setUpConfigurationExternalRefdkh2: ancestors [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalRef-dkh.2'.
  className := #'ConfigurationOfExternalRef'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef091:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'configurationExternalRef092:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'configurationExternalRef092:') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: ancestors)
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> setUpConfigurationIssue32 [
    "see https://github.com/dalehenrich/metacello-work/issues/32"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue32-dkh.1'.
    className := #'ConfigurationOfIssue32'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version10Issue47:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue47:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version09Issue32:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version09Issue32:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 339' }
MetacelloScriptingResource >> setUpConfigurationIssue339 [
  "see https://github.com/dalehenrich/metacello-work/issues/339"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfIssue339-dkh.1'.
  className := #'ConfigurationOfIssue339'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'version100Issue339:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'version100Issue339:') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 59' }
MetacelloScriptingResource >> setUpConfigurationIssue59 [
    "see https://github.com/dalehenrich/metacello-work/issues/59"

    "Use MetacelloVersionNumber instead of MetacelloSematicVersionNumber"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue59-dkh.1'.
    className := #'ConfigurationOfIssue59'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version10Issue59:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue59:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionNumberClass'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionNumberClass') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> setUpConfigurationIssue84 [
    "see https://github.com/dalehenrich/metacello-work/issues/84"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfIssue84-dkh.1'.
    className := #'ConfigurationOfIssue84'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version10Issue84:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10Issue84:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> setUpConfigurationNextedIssue84 [
    "see https://github.com/dalehenrich/metacello-work/issues/84"

    "self reset"

    | versionInfo |
    versionInfo := self setUpConfigurationNextedIssue84dkh1.
    self setUpConfigurationNextedIssue84dkh2: {versionInfo}
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> setUpConfigurationNextedIssue84dkh1 [
    "see https://github.com/dalehenrich/metacello-work/issues/84"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfNestedIssue84-dkh.1'.
    className := #'ConfigurationOfNestedIssue84'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version10NestedIssue84:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10NestedIssue84:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionNumberClass'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionNumberClass') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> setUpConfigurationNextedIssue84dkh2: ancestors [
    "see https://github.com/dalehenrich/metacello-work/issues/84"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfNestedIssue84-dkh.2'.
    className := #'ConfigurationOfNestedIssue84'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'version10NestedIssue84:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'version10NestedIssue84:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionNumberClass'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionNumberClass') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: ancestors)
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> setUpConfigurationOfConflict [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfConflict-dkh.1'.
  className := #'ConfigurationOfConflict'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf20:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf20:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf21:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf21:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf10:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf10:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf11:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf11:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'conflictOf12:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'conflictOf12:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'customProjectAttributes'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternalIV [
    "see https://github.com/dalehenrich/metacello-work/issues/6"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfExternalIV-dkh.1'.
    className := #'ConfigurationOfExternalIV'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionOfIV:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionOfIV:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternalX [
    "self reset"

    | versionInfo |
    versionInfo := self setUpConfigurationOfExternalXdkh1.
    versionInfo := self setUpConfigurationOfExternalXdkh2: {versionInfo}
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> setUpConfigurationOfExternalXX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfExternalXX-dkh.1'.
    className := #'ConfigurationOfExternalXX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionOfXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionOfXX:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternalXXX [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfExternalXXX-dkh.1'.
    className := #'ConfigurationOfExternalXXX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionOfXXX:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionOfXXX:') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternalXdkh1 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalX-dkh.1'.
  className := #'ConfigurationOfExternalX'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'stableVersionOfX:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersionOfX:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfX090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfX090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'customProjectAttributes'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternalXdkh2: ancestors [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternalX-dkh.2'.
  className := #'ConfigurationOfExternalX'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'stableVersionOfX:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'stableVersionOfX:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'unstableVersionOfX:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'unstableVersionOfX:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfX090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfX090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfX091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfX091:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'customProjectAttributes'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: ancestors)
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternaldkh1 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternal-dkh.1'.
  className := #'ConfigurationOfExternal'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfExternal090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfExternal090:') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> setUpConfigurationOfExternaldkh2 [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'ConfigurationOfExternal-dkh.2'.
  className := #'ConfigurationOfExternal'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfExternal090:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfExternal090:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'versionOfExternal091:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'versionOfExternal091:') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external core' }
MetacelloScriptingResource >> setUpExternalCore [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'External-Core-dkh.1'.
  className := #'ExternalCore'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: true
    selector: 'externalAuthorName'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'externalAuthorName') asString).
  (MCMethodDefinition
    className: 'Object'
    classIsMeta: true
    selector: 'isExternal'
    category: '*external-core'
    timeStamp: ''
    source: (self class sourceCodeAt: #'isExternal') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'external core' }
MetacelloScriptingResource >> setUpExternalCoreX [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'External-CoreX-dkh.1'.
  className := #'ExternalCoreX'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: true
    selector: 'externalAuthorName'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'externalAuthorName') asString).
  (MCMethodDefinition
    className: 'Object'
    classIsMeta: true
    selector: 'isExternal'
    category: '*external-corex'
    timeStamp: ''
    source: (self class sourceCodeAt: #'isExternal') asString)}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'invalid configurations' }
MetacelloScriptingResource >> setUpInvalidConfigurations [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'ConfigurationOfInvalidConfigurations-dkh.1'.
    className := #'ConfigurationOfInvalidConfigurations'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration10:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration10:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration20:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration20:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'invalidConfiguration30:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'invalidConfiguration30:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpIssue399 [
  | versionInfo |
  versionInfo := self setUpIssue399CoreSampledkh1.
  versionInfo := self setUpIssue399CoreSampledkh2: {versionInfo}.
  self
    setUpIssue399CoreExternaldkh1;
    setUpBaselineIssue399;
    setUpBaselineIssue399Cypress;
    yourself
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpIssue399CoreExternaldkh1 [
  "two packages with same name and version number, but a different class created."

  "this on goes into the external repository"

  "add first package to the package cache, then load a baseline in repo with other package"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'Issue399-Core-dkh.1'.
  className := #'Issue399External'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '')}.
  externalRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpIssue399CoreSampledkh1 [
  "two packages with same name and version number, but a different class created."

  "this on goes into the sample repository"

  "add first package to the package cache, then load a baseline in repo with other package"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'Issue399-Core-dkh.1'.
  className := #'Issue399Sample'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '')}.
  sampleRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'issue 399' }
MetacelloScriptingResource >> setUpIssue399CoreSampledkh2: ancestors [
  "two packages with same name and version number, but a different class created."

  "this on goes into the sample repository"

  "add first package to the package cache, then load a baseline in repo with other package"

  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference name: 'Issue399-Core-dkh.2'.
  className := #'Issue399Sample'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'Object'
    category: reference packageName
    instVarNames: #()
    comment: '')}.
  sampleRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: ancestors)
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #locking }
MetacelloScriptingResource >> setUpLockConfigurations [
  "self reset"

  | reference className definitionArray versionInfo |
  reference := GoferVersionReference
    name: 'ConfigurationOfLockConfigurations-dkh.1'.
  className := #'ConfigurationOfLockConfigurations'.
  definitionArray := {(MCOrganizationDefinition
    categories: (Array with: reference packageName asSymbol)).
  (MCClassDefinition
    name: className
    superclassName: #'ConfigurationOf'
    category: reference packageName
    instVarNames: #()
    comment: '').
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration10:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration10:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration11:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration11:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration12:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration12:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration13:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration13:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration14:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration14:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'lockConfiguration15:'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'lockConfiguration15:') asString).
  (MCMethodDefinition
    className: className asString
    classIsMeta: false
    selector: 'customProjectAttributes'
    category: 'cat'
    timeStamp: ''
    source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
  configurationRepository
    basicStoreVersion:
      (MCVersion new
        setPackage: (MetacelloTestsMCPackage new name: reference packageName)
        info:
          (versionInfo := MCVersionInfo
            name: reference name
            id: UUID new
            message: 'This is a mock version'
            date: Date today
            time: Time now
            author: reference author
            ancestors: #())
        snapshot: (MCSnapshot fromDefinitions: definitionArray)
        dependencies: #()).
  ^ versionInfo
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> setUpMarianosImage [
    "className: test case
	seehttps://github.com/dalehenrich/metacello-work/issues/24"

    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'MarianosImage-dkh.1'.
    className := #'MarianosImage'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'ConfigurationOf'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'versionOfMariano:'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'versionOfMariano:') asString).
    (MCMethodDefinition
        className: className asString
        classIsMeta: false
        selector: 'customProjectAttributes'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'customProjectAttributes') asString)}.
    configurationRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #running }
MetacelloScriptingResource >> setUpRepositories [
    "self reset"

    configurationRepository := MCDictionaryRepository new.
    externalRepository := MCDictionaryRepository new.
    sampleRepository := MCDictionaryRepository new
]

{ #category : #'sample repository' }
MetacelloScriptingResource >> setUpSampleCore [
    "self reset"

    | reference className definitionArray versionInfo |
    reference := GoferVersionReference name: 'Sample-CoreX-dkh.1'.
    className := #'SampleCoreX'.
    definitionArray := {(MCOrganizationDefinition categories: (Array with: reference packageName asSymbol)).
    (MCClassDefinition
        name: className
        superclassName: #'Object'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: className asString
        classIsMeta: true
        selector: 'sampleAuthorName'
        category: 'cat'
        timeStamp: ''
        source: (self class sourceCodeAt: #'sampleAuthorName') asString).
    (MCClassDefinition
        name: #'Object'
        superclassName: #'ProtoObject'
        category: reference packageName
        instVarNames: #()
        comment: '').
    (MCMethodDefinition
        className: 'Object'
        classIsMeta: true
        selector: 'isSample'
        category: '*sample-core'
        timeStamp: ''
        source: (self class sourceCodeAt: #'isSample') asString)}.
    sampleRepository
        basicStoreVersion:
            (MCVersion new
                setPackage: (MetacelloTestsMCPackage new name: reference packageName)
                info:
                    (versionInfo := MCVersionInfo
                        name: reference name
                        id: UUID new
                        message: 'This is a mock version'
                        date: Date today
                        time: Time now
                        author: reference author
                        ancestors: #())
                snapshot: (MCSnapshot fromDefinitions: definitionArray)
                dependencies: #()).
    ^ versionInfo
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> stableVersionOfX: spec [
    "self reset"

    <symbolicVersion: #'stable'>
    spec for: #'common' version: '0.9.0'
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> unstableVersionOfX: spec [
  "self reset"

  <symbolicVersion: #'unstable'>
  spec for: #'common' version: '0.9.1'
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> version09Issue32: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>baselineOfIssue32:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]
]

{ #category : #'issue 339' }
MetacelloScriptingResource >> version100Issue339: spec [
  "self reset"

  <version: '1.0.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>version100Issue339:'.
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec package: 'Issue339' with: 'Issue339-JanKurs.1' ]
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> version10Issue47: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>version10Issue47:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34'.
            spec baseline: 'Issue32' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]
]

{ #category : #'issue 32' }
MetacelloScriptingResource >> version10Issue59: spec [
    <version: '1.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>version10Issue59:'.
            spec author: 'dkh'.
            spec timestamp: '6/18/2012 14:34'.
            spec baseline: 'Issue32' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'Issue32' with: [ spec repository: 'dictionary://Metacello_External_Test_Repository' ] ]
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> version10Issue84: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'release'.
            spec description: 'MetacelloScriptingResource>>version10Issue84:'.
            spec author: 'dkh'.
            spec timestamp: '7/24/2012 19:23'.
            spec
                configuration: 'NestedIssue84'
                with: [ 
                    spec
                        version: '1.0.0';
                        repository: 'dictionary://Metacello_Config_Test_Repository' ] ]
]

{ #category : #'issue 84' }
MetacelloScriptingResource >> version10NestedIssue84: spec [
    <version: '1.0.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>version10NestedIssue84:'.
            spec author: 'dkh'.
            spec timestamp: '7/24/2012 19:23'.
            spec repository: 'dictionary://Metacello_Configuration_Test_Repository'.
            spec package: 'External-CoreX' ]
]

{ #category : #'issue 59' }
MetacelloScriptingResource >> versionNumberClass [
    ^ MetacelloVersionNumber
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfExternal090: spec [
  <version: '0.9.0'>
  spec
    for: #'common'
    do: [ 
      spec description: 'MetacelloScriptingResource>>versionOfExternal090:'.
      spec repository: 'dictionary://Metacello_Configuration_Test_Repository'.
      spec
        package: 'External-Core' with: 'External-Core-dkh.1';
        yourself ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfExternal091: spec [
  <version: '0.9.1'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'development'.
      spec description: 'MetacelloScriptingResource>>versionOfExternal091:'.
      spec author: 'dkh'.
      spec timestamp: '5/4/2012 14:16'.
      spec
        baseline: 'External'
        with: [ 
          spec
            repository:
              'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                , '/repository' ] ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfIV: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>baselineOfIV:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'baseline:with:' }
MetacelloScriptingResource >> versionOfMariano: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfMariano:'.
            spec author: 'dkh'.
            spec timestamp: '5/31/2012 16:04' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfX090: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfX090:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfX091: spec [
    <version: '0.9.1'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfX091:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ spec baseline: 'ExternalX' with: [ spec repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfXX: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfXX:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16' ].
    spec
        for: #'custom'
        do: [ 
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]
]

{ #category : #'external configurations' }
MetacelloScriptingResource >> versionOfXXX: spec [
    <version: '0.9.0'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'development'.
            spec description: 'MetacelloScriptingResource>>versionOfXXX:'.
            spec author: 'dkh'.
            spec timestamp: '5/4/2012 14:16'.
            spec
                baseline: 'External'
                with: [ spec repository: 'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA , '/repository' ] ]
]
