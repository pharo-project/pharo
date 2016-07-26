initialize
initializeFrom: defaultProvider
	"Initialize the receiver from a default provider"
	self hResizing: defaultProvider hResizing.
	self vResizing: defaultProvider vResizing.
	self disableTableLayout: defaultProvider disableTableLayout.