bootstrapping
loadPackageVersion: aFileName
	
	[ 	('Loading ', aFileName asString) logCr.
		"Use snapshot install instead of load to force loading.
		Otherwise monticello finds no changes and does not load anything."
		(self localRepository loadVersionFromFileNamed: aFileName) load.
	] on: MCMergeOrLoadWarning do: [:warning |
		('Warning: ', warning messageText asString) logCr.
		warning resume: true ]