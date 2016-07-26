initialization
initialize

	super initialize.
	
	history := NavigationHistory new.
	inspector := nil asValueHolder.
	
	self initializeShortcuts.
	inspector whenChangedDo: [ :new :old | 
		self updateInspectorFrom: old ].
	self announcer
		when: WidgetBuilt send: #setRefreshedToTrue to: self.
	
	canBeRefreshed := false.
	self whenWindowChanged: [ :w |
		w whenClosedDo: [ self inspector close ] ].