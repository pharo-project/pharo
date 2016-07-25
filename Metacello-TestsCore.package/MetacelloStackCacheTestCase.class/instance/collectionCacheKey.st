tests
collectionCacheKey

	| key |
	key := Array with: self class with: (Array with: #x with: #y).
	^MetacelloPlatform current 
		stackCacheFor: #collection
		at: key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 5 
				ifTrue: [ cache at: key put: value ]
				ifFalse: [ 
					value := value + 1.
					cache at: #x put: value ].
			self collectionCacheKey ]
