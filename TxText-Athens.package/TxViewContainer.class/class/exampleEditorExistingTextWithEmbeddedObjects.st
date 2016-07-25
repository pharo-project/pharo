examples
exampleEditorExistingTextWithEmbeddedObjects
"self exampleEditorExistingTextWithEmbeddedObjects
"
	| view wrapper text |

	text := 'abciuwhfi whifu wehifuhweifu wi ' asTxModel.

	1 to: 1000 do: [:i | 
	text endPosition insert: i asString, ' empty bottles on the bar'.
	text endPosition insertObject:  ((SimpleButtonMorph newWithLabel: 'DEADBEEF' ) color: Color random ) .
	text endPosition insert: 'uwe hfiuweh fiue', String cr.
	(text endPosition moveLeft selectTo: (text endPosition moveLeft: 5)) applyAttribute: (TxForeColorAttribute with: Color random).
	text endPosition insert: '-------', String cr.
	].

	view := TxViewContainer new.

"	view addDecorator: (TxViewDecoratorMorph new placeTop color: Color red;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeLeft  color: Color blue;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeBottom color: Color green;yourself)."
	view addDecorator: (TxScrollbarDecorator new placeRight color: Color blue;yourself).
	view addDecorator: (TxScrollbarDecorator new placeBottom color: Color green;yourself).
	view editText: text.
	view wrapText: false.
	view hResizing: #spaceFill ; vResizing: #spaceFill .
	view layoutFrame: LayoutFrame identity.
	
	wrapper := AthensWrapMorph new.
	wrapper layoutPolicy: ProportionalLayout new.
	wrapper addMorph: view.
	
	wrapper openInWindow extent: 400@400.


