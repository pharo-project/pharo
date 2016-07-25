I'm a package dependency graph. 
To build the dependency graph among packages, you have just to give a set of PDPackage on entry. Then :

	- use the message computeStaticDependencies messages to retrieve all the dependency among the packages.
	- use the message removeInternalDependencies if you want to remove all the internal dependency.
	- finally use combineDependencies to combine each dependency which have the same source and the target into 	composite dependency.
		