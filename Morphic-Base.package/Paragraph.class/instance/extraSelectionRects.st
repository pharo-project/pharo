selection
extraSelectionRects
	"Return an array of rectangles representing the findReplace and the secondary selection regions."
	^ Array streamContents: [:strm |
		extraSelectionBlocks 
			ifNotNil: [:blocks | 
				blocks do: [:selBlock | strm nextPutAll: (self selectionRectsFrom: selBlock first to: selBlock last)]]]
