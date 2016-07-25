accessing
versionList: aVersionList
	"Select versions to display and give them to the list model."

	| selectedVersions |
	selectedVersions := allVersionsCheckBox state
		ifTrue: [ aVersionList ]
		ifFalse: [ 
			aVersionList
				select: [ :version | version isStable or: [ version isDevelopment or: [ version isCurrentVersion or: [ version isBleedingEdge ] ] ] ] ].
	self versionList items: selectedVersions