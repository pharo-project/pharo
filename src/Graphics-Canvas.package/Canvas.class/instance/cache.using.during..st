drawing-support
cache: aRectangle using: aCache during: aBlock 
	"Cache the execution of aBlock by the given cache.
	Note: At some point we may want to actually *create* the cache here;
		for now we're only using it."

	(aCache notNil 
		and: [(aCache isForm) and: [aCache extent = aRectangle extent]]) 
			ifTrue: [^self paintImage: aCache at: aRectangle origin].
	aBlock value: self