instance-creation
data: aData from: aDataSource
	^ self new
		dataSource: aDataSource;
		data: aData;
		yourself