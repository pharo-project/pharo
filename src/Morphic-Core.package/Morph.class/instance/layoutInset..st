layout-properties
layoutInset: aNumber
	"Return the extra inset for layouts"
	self assureTableProperties layoutInset: aNumber.
	self layoutChanged.