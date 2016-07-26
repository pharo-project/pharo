*GT-InspectorExtensions-Core
gtScrollerWithWrappingAndNoSpaceFilling
	"
	self gtScrollerWithWrappingAndNoSpaceFilling openInWindow
	"

	<gtExample>
	| scrollpane pane |
	scrollpane := GeneralScrollPane new.
	scrollpane changeScrollerTableLayout.
	pane := PanelMorph new
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	scrollpane scrollTarget: pane.
	Object methods
		do: [ :each | 
			| methodLabel |
			methodLabel := each selector asTextMorph lock 
				backgroundColor: Color lightBlue.
				
			pane addMorph: methodLabel ].
	scrollpane
		hResizing: #shrinkWrap;
		height: 400.
	^ scrollpane