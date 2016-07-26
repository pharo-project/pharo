api repository shortcuts
bitbucketUser: userName project: projectName commitish: commitish path: path
  | branchOrCommitOrTag |
  branchOrCommitOrTag := commitish.
  branchOrCommitOrTag isEmpty
    ifTrue: [ branchOrCommitOrTag := 'master' ].
  self
    repository:
      'bitbucket://' , userName , '/' , projectName , ':' , branchOrCommitOrTag , '/'
        , path