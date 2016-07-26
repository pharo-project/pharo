tests
testPrimeStackCache

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