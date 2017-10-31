issue 215
baselineIssue215: spec
  <baseline>
  spec
    for: #'common'
    do: [ 
      spec description: self className , '>>baselineGithubReferenceV:'.
      spec
        baseline: 'External'
          with: [ 
              spec
                repository:
                  'github://dalehenrich/external:' , MetacelloScriptingResource externalCustomSHA
                    , '/repository' ];
        import: 'External' provides: #('External-Core' 'External-Tests') ]