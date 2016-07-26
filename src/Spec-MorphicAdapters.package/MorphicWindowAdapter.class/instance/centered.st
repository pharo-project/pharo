protocol
centered

	self widgetDo: [ :w | 
		w
			fullBounds;
			position: Display extent - w extent // 2 ]