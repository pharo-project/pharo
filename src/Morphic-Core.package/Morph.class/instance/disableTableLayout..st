layout-properties
disableTableLayout: aBool
	"Layout specific. Disable laying out the receiver in table layout"
	self assureLayoutProperties disableTableLayout: aBool.
	self layoutChanged.