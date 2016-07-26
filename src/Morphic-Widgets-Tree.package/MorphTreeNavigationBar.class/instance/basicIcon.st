accessing
basicIcon
	| button |
	^ (button := IconicButton new)
		clipSubmorphs: true;
		color: self pagerColor;		
		on: #mouseEnter send: #value to: [button borderColor: self pagerColor muchDarker];
		on: #mouseLeave send: #value to: [button borderColor: self pagerColor];
		target: self;
		layoutPolicy: TableLayout new; 
		listDirection: #leftToRight; 
		listCentering: #leftCenter; 
		wrapCentering: #center;
		layoutInset: 2@0;
		cellInset: 2@0;
		borderWidth: 1;
		borderColor: self pagerColor;
		yourself
	