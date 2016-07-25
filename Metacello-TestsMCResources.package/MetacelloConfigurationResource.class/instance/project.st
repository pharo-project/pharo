ConfigurationOfFoo
project
	"self reset"

	| constructor project |
	"Construct Metacello project"
	project := MetacelloMCProject new.
	"Allow for customization of #projectAttributes"
	project projectAttributes: MetacelloConfigurationResource projectAttributes.
	MetacelloVersionConstructor on: self project: project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: #linear.
	^project
