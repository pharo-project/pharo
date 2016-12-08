initialize-release
loadInstructions: loadInstructions sourceRepository: sourceRepositoryUrl

	| fetcher |
	fetcher := PBCargoFetcher new 
		loadInstructions: loadInstructions;
		sourceRepository: sourceRepositoryUrl;
		yourself.
	self definitionFetcher: fetcher