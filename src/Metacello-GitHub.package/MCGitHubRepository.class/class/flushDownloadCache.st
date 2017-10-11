initialization
flushDownloadCache
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  "self flushDownloadCache"

  DownloadCache := nil