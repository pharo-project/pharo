issue 283
projectVersion20Issue283: spec
  <version: '2.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec
        configuration: 'ProjectIssue283'
        with: [ 
          spec
            version: #'bleedingEdge';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]