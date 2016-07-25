initialization
initialize
	textConverterHolder := SourceMethodConverter new asValueHolder.
	model := AbstractTool new.
	topologicSortHolder := true asValueHolder.
	super initialize.
	askOkToClose := true asValueHolder.
	self windowIcon: self taskbarIcon.
	self registerToAnnouncements.
	self announcer when: WidgetBuilt send: #buildUpdateTitle to: self