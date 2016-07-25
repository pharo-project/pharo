as yet unclassified
buildWidget
	| view wrapper |
	
	view := TxViewContainer new.
	view addDecorator: (TxScrollbarDecorator new placeRight color: Color blue;yourself).
	view addDecorator: (TxScrollbarDecorator new placeBottom color: Color green;yourself).
	view useEditor.
	view wrapText: true.
	
	view hResizing: #spaceFill ; vResizing: #spaceFill .
	view layoutFrame: (LayoutFrame identity).
	
	wrapper := AthensWrapMorph new.
	wrapper layoutPolicy: ProportionalLayout new.
	wrapper addMorph: view.
	wrapper hResizing: #spaceFill ; vResizing: #spaceFill .

	^ wrapper
