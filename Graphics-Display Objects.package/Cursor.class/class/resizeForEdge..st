instance creation
resizeForEdge: aSymbol
	"Cursor resizeForEdge: #top"
	"Cursor resizeForEdge: #bottomLeft"
	^self perform: ('resize', aSymbol first asString asUppercase, (aSymbol copyFrom: 2 to: aSymbol size)) asSymbol.