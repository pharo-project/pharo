load and saving
save: aDictionary
	self isEnabled ifFalse: [ ^ self ].
	self delete.
	self ensureDirectory.
	self preferences writeStreamDo: [ :aPreferenceStream | 
			self save: aDictionary into: aPreferenceStream ]