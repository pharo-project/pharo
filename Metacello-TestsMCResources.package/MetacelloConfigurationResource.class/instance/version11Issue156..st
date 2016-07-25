Issue 115
version11Issue156: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '1.1.0'>
  spec
    for: #'common'
    do: [ 
      spec
        baseline: 'Goo'
          with: [ spec repository: 'dictionary://Metacello_MczConfiguration_Test_Repository' ];
        yourself ]