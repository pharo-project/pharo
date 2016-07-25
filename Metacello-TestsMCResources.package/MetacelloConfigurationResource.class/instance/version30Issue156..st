Issue 115
version30Issue156: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '3.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '5.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]