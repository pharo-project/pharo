dispatch
heightToDisplayInTree: aTree
	"Return the width of my representation as a list item"
	
	self extent ifNotNil: [:ex | ^ ex y ].
	self initialExtent ifNotNil: [:ex | ^ ex y ].
	
	self widget ifNil: [ self buildWithSpec ].
	^ self widget heightToDisplayInTree: aTree