SmalllintManifestChecker is responsible for running a set of rules on a given subsystem.

| rules checker | 
rules := RBCompositeLintRule allGoodRules resetResult.

"the resetResult is necessary because it will build a knid of cache of the result
and this cannot be done automatically (for example you may want to run several rules without invalidating the results). "

checker := SmalllintManifestChecker new
	runRules: rules onPackage: (RPackageOrganizer default packageNamed: #'Manifest-Core').