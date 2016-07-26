running
setUp
	aPackage := DAPackage on: (RPackageSet named: 'Collections-Abstract'). 
	aSecondPackage := DAPackage on: (RPackageSet named: 'Collections-Arithmetic').
	packageA := (DAPackage on: (RPackageSet named: 'A')).
	packageB := (DAPackage on: (RPackageSet named: 'B')).
	packageC := (DAPackage on: (RPackageSet named: 'C')).
	aPackage add: (DAInheritanceDependency from:aPackage to:aSecondPackage).