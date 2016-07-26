layout-properties
cellInset: aNumber
	"Layout specific. This property specifies an extra inset for each cell in the layout."
	self assureTableProperties cellInset: aNumber.
	self layoutChanged.