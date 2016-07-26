actions
diff
	| ancestorVersion |
	self pickAncestor ifNotNil:
		[:ancestor |
		ancestorVersion := self version workingCopy repositoryGroup versionWithInfo: ancestor.
		(self version asDiffAgainst: ancestorVersion) open]