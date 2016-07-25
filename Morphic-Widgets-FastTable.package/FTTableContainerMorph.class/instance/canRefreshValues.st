testing
canRefreshValues
	^ self needsRefreshExposedRows and: [ self owner isNotNil and: [ self owner hasDataSource ] ]