public api
run
	self applyPatches.
	self initializeClasses.
	self runStartup.
	self reloadBootstrapPackages.
	"self loadSUnitPackage.
	self loadTestPackages"