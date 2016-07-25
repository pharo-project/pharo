ConfigurationOfFoo
baselineGoo300Issue156Configuration: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <version: '3.0.0'>
  spec
    for: #'common'
    do: [ 
      spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
      spec
        package: 'GoferGoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar.branch-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.30';
        package: 'GoferBeau' with: 'GoferBeau-dkh.53';
        yourself ]