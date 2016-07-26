I am a PDPackage and i represent a package (RPackageSet) and all his dependencies (PDPackageDependency).

Instance variables :

	- dependencies : a collection of PDPackageDependency (actually the nodes of a graph dependency, from a source PDPackage to a target PDPackage)
	- included : says if the PDPackage is included or not in the set of packages at the beginning.
	- rpackage : the instance of asRPackageSet 	
	- inStack : useful for tarjan algorithm and cycle algorithm. It avoid stack access
	- tarjanIndex and tarjanLowLink : integer for the tarjan algorithm.
	- bfsParent : see cycle algorithm
	- seen : says if all dependencies have been added to the Package