column drawing
headerButton
	self header: ((MorphTreeColumnButton new) 
		layoutPolicy: RowLayout new; 
		listDirection: #leftToRight; 
		wrapCentering: #center;
		cellInset: 5@0;
		layoutInset: 5@0;
		yourself)