running
exportBootPackagesFolder

	'boot-packages' asFileReference ensureDeleteAll.
	definitionFetcher
		exportAllMczTo: 'boot-packages';
		exportAllToStFiles: 'boot-packages'.