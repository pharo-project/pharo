initialization
initialize
  "self initialize"

	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [ :c | c default registerNetworkClassNamed: self name ]
		ifNil:[ Smalltalk addToStartUpList: self ].
  self flushDownloadCache.
  self resetCacheDirectoryIfInvalid