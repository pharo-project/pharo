running
setUp
	aPackageRelationGraph := DAPackageRelationGraph new.
	packageCollectionAbstract := DAPackage on: (RPackageSet named:'Collections-Abstract').
	packageStrings := (DAPackage on:(RPackageSet named: 'Collections-Strings')).
	packageKernel := (DAPackage on:(RPackageSet named: 'Kernel')).
	packageRegexCore := (DAPackage on:(RPackageSet named: 'Regex-Core')).
	packageCollectionsSequenceable := (DAPackage on:(RPackageSet named: 'Collections-Sequenceable')).
	packagePackageDependencies := (DAPackage on:(RPackageSet named:  #'Tool-DependencyAnalyser')).
	packageA := (DAPackage on: (RPackageSet named: 'A')).
	packageB := (DAPackage on: (RPackageSet named: 'B')).
	packageC := (DAPackage on: (RPackageSet named: 'C')).
	packageD := (DAPackage on: (RPackageSet named: 'D')).