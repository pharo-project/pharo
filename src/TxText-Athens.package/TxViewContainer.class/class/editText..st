convenience
editText: aText
	| view wrapper |

	view := self new.

	view addDecorator: (TxScrollbarDecorator new placeRight color: Color blue;yourself).
	view addDecorator: (TxScrollbarDecorator new placeBottom color: Color green;yourself).
	view editText: aText.
	view wrapText: false.
	view hResizing: #spaceFill ; vResizing: #spaceFill .
	view layoutFrame: LayoutFrame identity.
	
	wrapper := AthensWrapMorph new.
	wrapper layoutPolicy: ProportionalLayout new.
	wrapper addMorph: view.
	
	^ wrapper openInWindow extent: 400@400.


