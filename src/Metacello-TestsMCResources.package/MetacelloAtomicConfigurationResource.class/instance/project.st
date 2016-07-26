ConfigurationOfAtomicFoo
project
	"self reset"

	| constructor project |
	"Construct Metacello project"
	constructor := MetacelloVersionConstructor on: self.
	project := constructor project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: #atomic.
	^project
