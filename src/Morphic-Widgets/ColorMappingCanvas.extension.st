Extension { #name : 'ColorMappingCanvas' }

{ #category : '*Morphic-Widgets' }
ColorMappingCanvas >> rubParagraph: paragraph bounds: bounds color: c [
	"Draw the given paragraph"

	self myCanvas rubParagraph: paragraph bounds: bounds color: (self mapColor: c)
]
