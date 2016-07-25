tests
testClearCache

	MetacelloPlatform current 
		stackCacheFor: #cached
		at: #key 
		doing: [:cache | | value |
			cache at: #x put: 1.
			self assert: (cache at: #x ifAbsent: []) == 1.
			MetacelloClearStackCacheNotification signal.
			self assert: (cache at: #x ifAbsent: []) == nil ]
