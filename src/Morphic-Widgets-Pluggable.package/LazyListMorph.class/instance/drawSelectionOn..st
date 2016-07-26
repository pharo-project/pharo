drawing
drawSelectionOn: aCanvas
	"Draw the selection background."
	
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	self drawBackgroundForSelectedRow: selectedRow on: aCanvas
