A PDPackageRelationGraphDiff do the diff between two PDPackageRelationGraph (an older and a newer) to detect :

 - which packages added/removed : packagesDiff.
 - which dependent packages added/removed from a package : dependentPackagesDiff.
 - which dependencies added/removed from a dependent package.