tests
mixedStack

	^MetacelloPlatform current 
		stackCacheFor: #mixed
		at: #key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 3 ifTrue: [ ^value ].
			value := value + 1.
			cache at: #x put: value.
			self assert: self directReturnOfValue == 6.
			self assert: self mixedStackCall == 4.
			self mixedStack ]
