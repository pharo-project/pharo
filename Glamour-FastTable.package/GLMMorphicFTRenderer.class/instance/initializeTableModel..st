initialization
initializeTableModel: aPresentation
	tableModel := (self createDataSourceFrom: aPresentation)
		glamourPresentation: aPresentation;
		yourself.
	self specificDataSourceInitializiation