issue 215
configuration098Issue215: spec
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