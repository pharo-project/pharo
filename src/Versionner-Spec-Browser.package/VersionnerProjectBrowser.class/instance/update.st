refreshing
update
	"Update the project details according to selected configuration and version."

	versionList selectedItem
		ifNil: [ projectPanel project: nil ]
		ifNotNil: [ :selectedVersion | 
			project := MTProject 
						newFromVersion: selectedVersion version
						inConfiguration: configuration configurationClass.
			projectPanel project: project.					
			repository text: project repository.
			versionDescription text: project version description.
			versionCommitMessage label: 'Commited: ' , project version author , ' - ' , project version timestamp ]