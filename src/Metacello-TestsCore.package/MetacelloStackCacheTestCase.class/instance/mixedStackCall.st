tests
mixedStackCall

	^MetacelloPlatform current 
		stackCacheFor: #mixedStack
		at: #key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 3 ifTrue: [ ^value ].
			value := value + 1.
			cache at: #x put: value.
			self assert: self cachedReturnOfValue == 6.
			self mixedStackCall ]
