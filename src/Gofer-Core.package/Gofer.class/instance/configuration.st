references
configuration
	"Add a default Configuration package by using the repository name.
	Assumes that there has been at least one repository set"
	^ self configurationOf: self repositories last project capitalized.