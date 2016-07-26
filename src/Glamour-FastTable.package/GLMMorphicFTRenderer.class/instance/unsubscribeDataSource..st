private
unsubscribeDataSource: aTableModel
	aTableModel announcer unsubscribe: self.
	tableMorph announcer unsubscribe: aTableModel