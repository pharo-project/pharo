Extension { #name : #ColorMappingCanvas }

{ #category : #'*Rubric-Editing-Core' }
ColorMappingCanvas >> rubParagraph: paragraph bounds: bounds color: c [
	"Draw the given paragraph"
	myCanvas
		rubParagraph: paragraph
		bounds: bounds
		color: (self mapColor: c)
]
