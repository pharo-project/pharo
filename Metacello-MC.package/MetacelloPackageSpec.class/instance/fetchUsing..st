loading
fetchUsing: mcLoader

	| fetchingSpecLoader |
	fetchingSpecLoader := mcLoader fetchingSpecLoader.
	fetchingSpecLoader
		linearLoadPackageSpecs: (Array with: self) 
		repositories: (fetchingSpecLoader repositoriesFrom: self repositorySpecs).