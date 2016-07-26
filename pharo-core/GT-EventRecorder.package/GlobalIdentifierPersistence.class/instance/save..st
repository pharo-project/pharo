load and saving
save: aDictionary
	self delete.
	self ensureDirectory.
	self preferences writeStreamDo: [ :aPreferenceStream | 
			self save: aDictionary into: aPreferenceStream ]