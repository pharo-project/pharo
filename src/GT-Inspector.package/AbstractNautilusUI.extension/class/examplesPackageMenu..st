*GT-Inspector-Examples-UI
examplesPackageMenu: aBuilder
	<contextMenu>
	<nautilusGlobalPackageMenu>
	^ GTExampleNautilus new
		onPackage: aBuilder;
		yourself