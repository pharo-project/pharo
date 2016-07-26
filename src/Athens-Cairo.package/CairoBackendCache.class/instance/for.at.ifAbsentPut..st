accessing
for: anObject at: key ifAbsentPut: aBlock

	| subCache |
	subCache := cache 
		at: anObject
		ifAbsentPut: [ WeakIdentityKeyDictionary new ].

	^ subCache at: key ifAbsentPut: aBlock