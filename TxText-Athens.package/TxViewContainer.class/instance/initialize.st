initialization
initialize
	super initialize.
	self layoutPolicy: ProportionalLayout new.
	self layoutInset: 0.
	decorators := OrderedCollection new.
	view := self newView.
	view 
		layoutFrame: LayoutFrame identity;
		hResizing: #spaceFill;
		vResizing: #spaceFill.

	self addMorph: view.
	
