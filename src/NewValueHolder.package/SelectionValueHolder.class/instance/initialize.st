initialization
initialize

	super initialize.
	index := 0 asValueHolder.
	selection := nil asValueHolder.
	
	index whenChangedSend: #valueChanged to: self.
	selection whenChangedSend: #valueChanged to: self.