accessing
defaultHeaderColor
	self flag: #todo. "I think we should deprecate all this headerColor stuff. This is 
	responsibility of data source, after all"
	^ Smalltalk ui theme fastTableHeaderColor