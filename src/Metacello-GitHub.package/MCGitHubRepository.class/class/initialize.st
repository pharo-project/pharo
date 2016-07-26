initialization
initialize
	SessionManager default 
		registerNetworkClassNamed: self name.
	
	self flushDownloadCache.
	self resetCacheDirectoryIfInvalid.