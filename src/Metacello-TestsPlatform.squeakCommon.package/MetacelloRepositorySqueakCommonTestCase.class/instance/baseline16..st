baselines
baseline16: spec
  <version: '1.6-baseline'>
  spec
    for: #'common'
    do: [ 
      spec
        package: 'Example-Core'
        with: [ 
          spec
            includes: 'Example-AddOn';
            file: 'Example-Core-anon.1';
            repository:
                'filetree://' , MCFileTreeFileUtils current default fullName , '/temp/repo' ] ]

