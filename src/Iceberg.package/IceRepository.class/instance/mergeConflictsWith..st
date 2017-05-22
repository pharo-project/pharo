actions
mergeConflictsWith: aCommitish
	"Use version merger to create a new version. 
	If all conflicts are resolved, commit it to the repository, 
	marking it as parent to the received commitish."
	
	| versionsChangedInCommit |
	versionsChangedInCommit := aCommitish versionsChangedSince: (self mergeBaseWith: aCommitish).

	(MCVersionMerger new
		addVersions: (versionsChangedInCommit collect: #mcVersion);
		merge)
		ifTrue: [ self commitPackagesAndMarkAsMerged: aCommitish ]

