editing
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the paragraph.
	Couple ParagraphEditor to Morphic keyboard events"
	| oldEditor oldParagraph oldText |
	oldEditor := self editor.
	oldParagraph := paragraph.
	oldText := oldParagraph text copy.

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	(oldParagraph == paragraph) ifTrue:[
		"this will not work if the paragraph changed"
		editor := oldEditor.     "since it may have been changed while in block"
	].
	self selectionChanged.  "Note new selection"
	(oldText = paragraph text and: [ oldText runs = paragraph text runs ])
		ifFalse:[ self updateFromParagraph ].