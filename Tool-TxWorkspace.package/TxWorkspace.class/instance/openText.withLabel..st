opening
openText: aText withLabel: aLabel
	| view wrapper |
	
	text := aText asTxModel.
	view := TxViewContainer new.

	view addDecorator: (TxScrollbarDecorator new placeRight color: Color blue;yourself).
	view addDecorator: (TxScrollbarDecorator new placeBottom color: Color green;yourself).
	view editText: text.
	view wrapText: true.
	view hResizing: #spaceFill ; vResizing: #spaceFill .
	view layoutFrame: LayoutFrame identity.
	
	view view onAnnouncement: TxContextMenuAnnouncement send: #buildContextMenu: to: self.
	editor := view view.
	
	self wireEvents.
	
	wrapper := AthensWrapMorph new.
	wrapper layoutPolicy: ProportionalLayout new.
	wrapper addMorph: view.
	
	^ (wrapper openInWindowLabeled: aLabel) extent: self defaultExtent.