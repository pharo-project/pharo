accessing
repositoryVersionString
  "for git-based network repos, answer the SHA associated with the download: a commit SHA or tag SHA"

  | versionComponents versionElement pathElements gitBasedPath repositoryDirPath projectDirPath projectDir projectVersionDir |
  repositoryDirPath := self directory fullName.
  projectDir := self class cacheDirectoryFor: self projectPath.
  projectVersionDir := MCFileTreeFileUtils current
    directoryFromPath: self projectVersion
    relativeTo: projectDir.
  projectDirPath := projectVersionDir fullName.
  (repositoryDirPath beginsWith: projectDirPath)
    ifFalse: [ ^ self projectVersion ].
  gitBasedPath := repositoryDirPath
    copyFrom: projectDirPath size + 2
    to: repositoryDirPath size.
  pathElements := gitBasedPath findTokens: '/'.
  versionElement := pathElements at: 1.
  versionComponents := versionElement findTokens: '-'.
  ^ versionComponents last