*Pharo30Bootstrap
written: aCGOPackageVersionReference
	| mcVersions |
	mcVersions :=  Cargo new 
		package: aCGOPackageVersionReference;
		mcVersions.
	^ self ficusEnvironmentFromMcVersions: mcVersions