accessing-packages
packagesWithContributors
	"answer a list of all packages contributing to the definition of classes in the environment"
	^ self classes 
		flatCollect: #packages
		as: Set