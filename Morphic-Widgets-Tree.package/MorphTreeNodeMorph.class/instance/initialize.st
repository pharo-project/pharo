initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	self
		layoutPolicy: TableLayout new;
		cellPositioning: #leftCenter;
		listDirection: #leftToRight;
		cellSpacing: #none;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		color: Color transparent.


