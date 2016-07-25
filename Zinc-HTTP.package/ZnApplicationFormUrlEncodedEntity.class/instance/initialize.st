initialization
initialize
	super initialize.
	fields := ZnMultiValueDictionary new.
	self contentType: self class designatedMimeType 