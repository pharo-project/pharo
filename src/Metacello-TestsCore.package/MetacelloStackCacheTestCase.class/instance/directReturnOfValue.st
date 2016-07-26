tests
directReturnOfValue

	^MetacelloPlatform current 
		stackCacheFor: #direct
		at: #key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 5 ifTrue: [ ^value ].
			value := value + 1.
			cache at: #x put: value.
			self directReturnOfValue ]
