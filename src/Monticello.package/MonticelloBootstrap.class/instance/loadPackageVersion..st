bootstrapping
loadPackageVersion: aFileName
	
	[ 	('Loading ', aFileName asString) logCr.
		(self localRepository loadVersionFromFileNamed: aFileName) load.
	] on: MCMergeOrLoadWarning do: [:warning |
		('Warning: ', warning messageText asString) logCr.
		warning resume: true ]