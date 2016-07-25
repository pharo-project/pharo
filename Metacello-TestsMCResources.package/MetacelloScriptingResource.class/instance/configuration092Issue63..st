issue 63
configuration092Issue63: spec
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