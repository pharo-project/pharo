initialization
dynamicLayout.
	self
		layout:
			(SpecLayout composed
				newRow: [ :row | 
					row
						add: #label origin: 0 @ 0 corner: 0.3 @ 1;
						add: #textInput origin: 0.3 @ 0 corner: 1 @ 1 ];
				yourself).