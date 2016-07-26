layout-properties
listDirection: aSymbol
	"Layout specific. This property describes the direction in which a list-like layout should be applied. Possible values are:
		#leftToRight
		#rightToLeft
		#topToBottom
		#bottomToTop
	indicating the direction in which any layout should take place"
	self assureTableProperties listDirection: aSymbol.
	self layoutChanged.