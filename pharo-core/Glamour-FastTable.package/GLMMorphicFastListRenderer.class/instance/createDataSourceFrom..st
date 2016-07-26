initialization
createDataSourceFrom: aPresentation
	^ aPresentation allowSeparators
		ifTrue: [ GLMFastListOutlineDataSource new ]
		ifFalse: [ GLMFastListDataSource new ]