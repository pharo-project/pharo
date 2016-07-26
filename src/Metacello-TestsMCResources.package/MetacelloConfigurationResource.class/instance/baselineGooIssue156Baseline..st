ConfigurationOfFoo
baselineGooIssue156Baseline: spec
  "https://github.com/dalehenrich/metacello-work/issues/156"

  <baseline>
  spec
    for: #'common'
    do: [ 
      spec
        package: 'GoferFoo' with: 'GoferFoo-lr.1';
        package: 'GoferBar' with: 'GoferBar.branch-lr.1';
        package: 'GoferFaux' with: 'GoferFaux-tg.30';
        package: 'GoferBeau' with: 'GoferBeau-dkh.53';
        yourself ]