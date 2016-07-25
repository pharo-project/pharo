querying
outgoingVersions
	| outgoingCommits |
	outgoingCommits := self repository outgoingCommits.
	^ self versions select: [ :version | outgoingCommits includes: version commit ]