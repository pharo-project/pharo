as yet unclassified
setLayoutSizingFor: aSymbol
	"Adjust the sizing for use within table layouts."

	(aSymbol = #left or: [aSymbol = #right])
		ifTrue: [self hResizing: #rigid; vResizing: #spaceFill].
	(aSymbol = #top or: [aSymbol = #bottom])
		ifTrue: [self hResizing: #spaceFill; vResizing: #rigid]