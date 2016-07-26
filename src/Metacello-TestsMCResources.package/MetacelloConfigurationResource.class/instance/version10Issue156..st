Issue 115
version10Issue156: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '1.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '3.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]