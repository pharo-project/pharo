private
cacheDirectory: aDirectory
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  CacheDirectory := aDirectory