startup loading
loadFromDefaultLocations
	self class allowStartupScript ifFalse: [ ^ self ].
	
	[ self load: self retrieveFilesStream ]
		on: StartupPreferencesLoadingError
		do: [ :ex | ex return ]