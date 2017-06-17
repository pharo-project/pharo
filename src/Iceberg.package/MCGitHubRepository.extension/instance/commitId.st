*Iceberg-Adapters
commitId
	"This kind of repo is associated to a specific commit that was downloaded, 
	sadly the only way to know it is to parse the directory name."
	| repoRoot |
	repoRoot := self repoPath isEmpty ifTrue: [ self directory ] ifFalse: [ self directory parent ].
	^ repoRoot basename copyAfterLast: $-.