rendering
render: aPresentation
	| container |
	container := GLMMorphic containerMorph.
	self initializeAnnoucementForPresentation: aPresentation.
	self initializeTableModel: aPresentation.
	self initializeTableMorph.
	self addFastTableIn: container.
	self installActionsOnModel: tableModel fromPresentation: aPresentation.
	self installKeystrokeActionsOnMorph: tableMorph fromPresentation: aPresentation.
	aPresentation selection ifNotNil: [ tableModel announcer suspendAll: self suspendedAnnouncements while: [ tableModel explicitSelection: aPresentation selection ] ].
	self initializeAnnouncementForDataSource.
	^ container