accessing
kernelPackageNames

	kernelPackageNames ifNotNil: [ ^ kernelPackageNames ]. 
	self ensureBaselineOfPharoBootstrap.
	^ kernelPackageNames := #BaselineOfPharoBootstrap asClass kernelPackageNames