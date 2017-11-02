Class {
	#name : #MetacelloStackCacheTestCase,
	#superclass : #TestCase,
	#category : 'Metacello-TestsCore'
}

{ #category : #tests }
MetacelloStackCacheTestCase >> cacheNil [

	^MetacelloPlatform current 
		stackCacheFor: #cacheNil
		at: #key 
		doing: [:cache | | value |
			cache at: #key put: nil.
			self cacheNil ]

]

{ #category : #tests }
MetacelloStackCacheTestCase >> cachedReturnOfValue [

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

]

{ #category : #tests }
MetacelloStackCacheTestCase >> collectionCacheKey [

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

]

{ #category : #tests }
MetacelloStackCacheTestCase >> directReturnOfValue [

	^MetacelloPlatform current 
		stackCacheFor: #direct
		at: #key 
		doing: [:cache | | value |
			value := cache at: #x ifAbsent: [ 0 ].
			value > 5 ifTrue: [ ^value ].
			value := value + 1.
			cache at: #x put: value.
			self directReturnOfValue ]

]

{ #category : #tests }
MetacelloStackCacheTestCase >> mixedStack [

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

]

{ #category : #tests }
MetacelloStackCacheTestCase >> mixedStackCall [

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

]

{ #category : #tests }
MetacelloStackCacheTestCase >> testCacheNil [

	| x |
	self assert: (x := self cacheNil) == nil
]

{ #category : #tests }
MetacelloStackCacheTestCase >> testCachedReturn [

	| x |
	self assert: (x := self cachedReturnOfValue) == 6
]

{ #category : #tests }
MetacelloStackCacheTestCase >> testClearCache [

	MetacelloPlatform current 
		stackCacheFor: #cached
		at: #key 
		doing: [:cache | | value |
			cache at: #x put: 1.
			self assert: (cache at: #x ifAbsent: []) == 1.
			MetacelloClearStackCacheNotification signal.
			self assert: (cache at: #x ifAbsent: []) == nil ]

]

{ #category : #tests }
MetacelloStackCacheTestCase >> testCollectionCacheKey [

	| x |
	self assert: 
		(Array with: self class with: (Array with: 'faux' with: 'foobar'))
			= (Array with: self class with: (Array with: 'faux' with: 'foobar')).
	self deny: 
		(Array with: self class with: (Array with: 'faux' with: 'foobar'))
			= (Array with: self class with: (Array with: 'faux' with: 'foobar' with: 'foobaz')).
	self assert: (x := self collectionCacheKey) == 6.

]

{ #category : #tests }
MetacelloStackCacheTestCase >> testDefaultDictionary [

	| defaultDictionary |
	defaultDictionary := Dictionary new.
	MetacelloPlatform current 
		useStackCacheDuring: [:dict | 
			self 
				cachedReturnOfValue;
				collectionCacheKey;
				cacheNil;
				directReturnOfValue;
				mixedStack;
				mixedStackCall ] 
		defaultDictionary: defaultDictionary.
	self assert: (defaultDictionary includesKey: #cached).
	self assert: (defaultDictionary includesKey: #cacheNil).
	self assert: (defaultDictionary includesKey: #collection).
	self assert: (defaultDictionary includesKey: #direct).
	self assert: (defaultDictionary includesKey: #mixed).
	self assert: (defaultDictionary includesKey: #mixedStack).

]

{ #category : #tests }
MetacelloStackCacheTestCase >> testDirectReturn [

	| x |
	self assert: (x := self directReturnOfValue) == 6
]

{ #category : #tests }
MetacelloStackCacheTestCase >> testMixedStack [

	| x |
	self assert: (x := self mixedStack) == 4
]

{ #category : #tests }
MetacelloStackCacheTestCase >> testPrimeStackCache [

	| cached collection cacheNil direct mixed mixedStack block defaultDictionary |
	defaultDictionary := Dictionary new.
	block := [ 
		cached := Dictionary new.
		collection := Dictionary new.
		cacheNil := Dictionary new.
		direct := Dictionary new.
		mixed := Dictionary new.
		mixedStack := Dictionary new.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self cachedReturnOfValue ] defaultDictionary: cached.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self collectionCacheKey ] defaultDictionary: collection.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self cacheNil ] defaultDictionary: cacheNil.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self directReturnOfValue ] defaultDictionary: direct.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self mixedStack ] defaultDictionary: mixed.
		MetacelloPlatform current useStackCacheDuring: [ :dict | self mixedStackCall ] defaultDictionary: mixedStack ].	

	"Test without priming stack cache ... each defaultDictionary is used independently"
	block value.
	self assert: ((cached at: #cached) at: #key) == 6.
	self assert: ((collection at: #collection) at: #x) == 6.
	self assert: ((cacheNil at: #cacheNil) at: #key) == nil.
	self assert: ((direct at: #direct) at: #x) == 6.
	self assert: ((mixed at: #cached) at: #key) == 6.
	self assert: ((mixed at: #mixed) at: #x) == 4.
	self assert: ((mixed at: #mixedStack) at: #x) == 4.
	self assert: ((mixed at: #direct) at: #x) == 6.
	self assert: ((mixedStack at: #cached) at: #key) == 6.
	self assert: ((mixedStack at: #mixedStack) at: #x) == 4.	

	"Prime stack cache with defaultDictionary .... all values should be stored there instead of individual defaults."
	MetacelloPlatform current primeStackCacheWith: defaultDictionary doing: block.
	self assert: ((defaultDictionary at: #cached) at: #key) == 6.
	self assert: ((defaultDictionary at: #collection) at: #x) == 6.
	self assert: ((defaultDictionary at: #cacheNil) at: #key) == nil.
	self assert: ((defaultDictionary at: #direct) at: #x) == 6.
	self assert: ((defaultDictionary at: #cached) at: #key) == 6.
	self assert: ((defaultDictionary at: #mixed) at: #x) == 4.
	self assert: ((defaultDictionary at: #mixedStack) at: #x) == 4.
	self assert: ((defaultDictionary at: #direct) at: #x) == 6.
	self assert: ((defaultDictionary at: #cached) at: #key) == 6.
	self assert: ((defaultDictionary at: #mixedStack) at: #x) == 4
]
