operations
baseSnapshot
       | baseVersion |
       baseVersion := self workingCopy repositoryGroup versionWithInfo: base.
       baseVersion ifNil: [ self error: 'Missing snapshot: ', self baseInfo name].
       ^ baseVersion snapshot