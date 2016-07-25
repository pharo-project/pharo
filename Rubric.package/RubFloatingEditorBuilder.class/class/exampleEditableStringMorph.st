example
exampleEditableStringMorph
	<example>
	| pane stringMorph edBuilder |
	pane := Morph new.
	pane
		color: Color white;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 5.
	stringMorph := StringMorph
		contents: 'Open the mini editor: [Shift+mouseDown on the string]; Update: [Enter|Cmd+s]; Abort: [Escape]'.
	stringMorph 
		position: 5 asPoint.
		pane addMorph: stringMorph.
	edBuilder := RubFloatingEditorBuilder new.
	edBuilder
		customizeEditorWith: [ :editor | 
			editor	 
				color: Color veryLightGray veryMuchLighter;
				bounds:
					(((stringMorph bounds withRight: pane right) withBottom: stringMorph bottom + 40)
						translateBy: 2 negated asPoint) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newContents |
			newContents := editedContents ifEmpty: [ 'Ahem! I need something here...' ].
			stringMorph contents: newContents.
			self inform: newContents ].
	stringMorph
		on: #mouseDown
		send: #value:
		to: [ :event | 
			event shiftPressed
				ifTrue: [ edBuilder openEditorWithContents: stringMorph contents ] ].
	(pane embeddedInMorphicWindowLabeled: 'Editable sm example')
		openInWorldExtent: (stringMorph extent translateBy:  30 @ 30)