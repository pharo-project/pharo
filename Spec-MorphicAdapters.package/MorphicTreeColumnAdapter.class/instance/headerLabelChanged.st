private
headerLabelChanged
	
	self widgetDo: [ :w | 
		w header label: self headerLabel.
		w container resizerChanged ]