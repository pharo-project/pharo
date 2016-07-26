initialization
initialize
	"Initialize the receiver."

	super initialize.
	selectedIndex := 0.
	self
		tabs: OrderedCollection new;
		wrapScrolling: false;
		visibleTabRange: (1 to: 0);
		controls: self newControls;
		roundedCorners: #(1 4);
		borderWidth: 0;
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: (self theme tabSelectorCellInsetFor: self)