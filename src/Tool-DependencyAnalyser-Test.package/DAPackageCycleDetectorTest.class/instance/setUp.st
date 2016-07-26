running
setUp
	aPackageCycleDetection := DAPackageCycleDetector new.
	packageA := (DAPackage on: (RPackageSet named: 'A')).
	packageB := (DAPackage on: (RPackageSet named: 'B')).
	packageC := (DAPackage on: (RPackageSet named: 'C')).
	packageD := (DAPackage on: (RPackageSet named: 'D')).
	packageE :=  (DAPackage on: (RPackageSet named: 'E')).
	cycleA := DAPackageCycle new.
	cycleB := DAPackageCycle new.
	cycleC := DAPackageCycle new.