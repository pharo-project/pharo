accessing
pdPackagesFrom: selectedItems
	^ selectedItems collect: [ :item | DAPackage on: (RPackageSet named: item content packageName asString) ]