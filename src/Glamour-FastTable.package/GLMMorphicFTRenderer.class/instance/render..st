rendering
render: aPresentation
	| container |
	container := GLMMorphic containerMorph.
	self initializeAnnoucementForPresentation: aPresentation.
	self initializeTableModel: aPresentation.
	self initializeTableMorph.
	self addFastTableIn: container.
	aPresentation selection ifNotNil: [ tableModel announcer suspendAll: self suspendedAnnouncements while: [ tableModel explicitSelection: aPresentation selection ] ].
	"No need to initialize the announcements here as setting the data source in a FTTableMorph will do it."
	"self initializeAnnouncementForDataSource."
	^ container