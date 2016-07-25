private
dataSourceUpdated: announcement
	tableModel ifNotNil: [ self unsubscribeDataSource: tableModel ].
	tableModel := announcement newDataSource.
	self initializeAnnouncementForDataSource