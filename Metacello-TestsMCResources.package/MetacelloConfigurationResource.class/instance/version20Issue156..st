Issue 115
version20Issue156: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '2.0.0'>
  spec
    for: #'common'
    do: [ 
      spec
        configuration: 'Goo'
          with: [ 
              spec
                versionString: '4.0.0';
                repository:
                    'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]