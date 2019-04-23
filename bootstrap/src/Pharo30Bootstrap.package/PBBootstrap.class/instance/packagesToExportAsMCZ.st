package-names
packagesToExportAsMCZ

	^ #(BaselineOfMetacello) , (#BaselineOfMetacello asClass allPackageNames)  , self monticelloPackageNames , self hermesPackageNames, self kernelPackageNames , self monticelloNetworkPackageNames