accessing settings
sharedRepositoriesLocation
	^ SharedRepositoriesLocation ifNil: [ 
		SharedRepositoriesLocation := (FileLocator home / #iceberg) asFileReference ]