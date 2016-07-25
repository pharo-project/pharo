api repository shortcuts
githubUser: userName project: projectName commitish: commitish path: path
  | branchOrCommitOrTag |
  branchOrCommitOrTag := commitish.
  branchOrCommitOrTag isEmpty
    ifTrue: [ branchOrCommitOrTag := 'master' ].
  self
    repository:
      'github://' , userName , '/' , projectName , ':' , branchOrCommitOrTag , '/'
        , path