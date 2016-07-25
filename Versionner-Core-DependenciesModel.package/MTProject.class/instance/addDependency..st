setting
addDependency: aDependency
	"Add a dependency to the configuration (will create a new baseline)."

	dependencies add: aDependency.
	aDependency parent: self.
	self announcer announce: (MTProjectModelChanged project: self).