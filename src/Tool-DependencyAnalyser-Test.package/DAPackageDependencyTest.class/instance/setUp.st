running
setUp
	| source target |
	source := DAPackage on: (RPackageSet named:'Kernel').
	target := DAPackage on: (RPackageSet named:'Collections-Abstract').
	aPackageDependency := DAPackageDependency from:source to:target.
	anInternalPackageDependency := DAPackageDependency from:source to:source.
	packageA := DAPackage on: (RPackageSet named:'A').
	packageB := DAPackage on: (RPackageSet named:'B').