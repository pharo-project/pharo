tests
cachedReturnOfValue

	^MetacelloPlatform current 
		stackCacheFor: #cached
		at: #key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 5 
				ifTrue: [ cache at: #key put: value ]
				ifFalse: [ 
					value := value + 1.
					cache at: #x put: value ].
			self cachedReturnOfValue ]
