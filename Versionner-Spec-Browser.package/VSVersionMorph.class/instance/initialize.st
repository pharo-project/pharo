initialization
initialize
	super initialize.	
	tags := OrderedCollection new.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		cellInset: 3;
		color: Color transparent;
		yourself.
	