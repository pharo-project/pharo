accessing - cache
cacheAt: aKey for: anElement ifAbsentPut: aBlock
	| elementCache |
	
	self shouldUseCache ifFalse: [ 
		^ aBlock value ].
	
	elementCache := self dataCacheFor: anElement.
	(elementCache includesKey: aKey) 
		ifTrue: [ ^ elementCache at: aKey ].
		
	^ aBlock value 
		ifNotNil: [ :anObject | elementCache at: aKey ifAbsentPut: [ anObject ] ]