accessing
downloadCache
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  DownloadCache ifNil: [ DownloadCache := Dictionary new ].
  ^ DownloadCache