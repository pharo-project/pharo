caching
useStackCacheDuring: aBlock defaultDictionary: defaultDictionary
	| dict |
	dict := MetacelloStackCacheNotification signal.
	dict == nil
		ifTrue: [ 
			dict := defaultDictionary == nil
				ifTrue: [ Dictionary new ]
				ifFalse: [ defaultDictionary ] ].
	[ ^ aBlock value: dict ]
		on: MetacelloStackCacheNotification , MetacelloClearStackCacheNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloStackCacheNotification)
				ifTrue: [ ex resume: dict ].
			(ex isKindOf: MetacelloClearStackCacheNotification)
				ifTrue: [ 
					| keys |
					keys := ex cacheNames.
					keys ifNil: [ keys := dict keys ].
					keys
						do: [ :k | 
							(dict includesKey: k)
								ifTrue: [ 
									| c |
									c := dict at: k.
									c keys do: [ :ck | c removeKey: ck ].
									dict removeKey: k ] ].
					ex resume ] ]