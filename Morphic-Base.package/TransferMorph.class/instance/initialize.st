initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	self layoutPolicy: TableLayout new.
	
	self listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 3;
		cellInset: 3;
		wrapCentering: #center;
		cellPositioning: #leftCenter.
		
	self borderStyle: (BorderStyle
		width: 1 color: Color lightGray).
		
	accepted := false.	
	copy := false.
	self on: #keyStroke send: #keyStroke: to: self