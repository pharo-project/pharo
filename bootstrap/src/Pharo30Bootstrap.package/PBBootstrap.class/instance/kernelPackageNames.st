accessing
kernelPackageNames

	kernelPackageNames ifNotNil: [ ^ kernelPackageNames ]. 
	self ensureBaselineOfPharoBootstrap.
	^ kernelPackageNames := #BaselineOfPharoBootstrap asClass project version packages collect: #name