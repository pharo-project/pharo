running
setUp
	aPDPackageCycle := DAPackageCycle new.
	packageA := DAPackage on: (RPackageSet named:'A').
	packageB := DAPackage on: (RPackageSet named:'B').
	packageC := DAPackage on: (RPackageSet named:'C').