examples
exampleOneLineEditor

	| view wrapper |

	view := TxViewContainer new.

"	view addDecorator: (TxViewDecoratorMorph new placeTop color: Color red;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeLeft  color: Color blue;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeBottom color: Color green;yourself)."
	view addDecorator: (TxScrollbarDecorator new placeBottom color: Color green;yourself).
	view useEditor.
	view wrapText: false.
	view hResizing: #spaceFill ; vResizing: #spaceFill .
	view layoutFrame: LayoutFrame identity.
	
	wrapper := AthensWrapMorph new.
	wrapper layoutPolicy: ProportionalLayout new.
	wrapper addMorph: view.
	
	wrapper openInWindow extent: 400@400.


