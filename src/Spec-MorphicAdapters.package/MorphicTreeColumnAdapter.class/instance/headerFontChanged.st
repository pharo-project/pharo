private
headerFontChanged
	
	self widgetDo: [ :w | 
		w header label: self headerLabel font: self headerFont.
		w container resizerChanged ]