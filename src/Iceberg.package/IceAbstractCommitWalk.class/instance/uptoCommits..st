walk definition
uptoCommits: commits
	commits do: [ :commit | self uptoCommit: commit  ]