building
build
	"build the dependency graph"
	self 
		computeStaticDependencies;
		removeInternalDependencies;
		combineDependencies