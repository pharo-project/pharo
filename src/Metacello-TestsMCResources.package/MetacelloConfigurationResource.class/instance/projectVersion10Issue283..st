issue 283
projectVersion10Issue283: spec
  <version: '1.0'>
  spec
    for: #'common'
    do: [ 
      spec blessing: #'release'.
      spec
        configuration: 'ProjectIssue283'
        with: [ 
          spec
            version: #'stable';
            repository: 'dictionary://Metacello_Configuration_Test_Repository' ] ]