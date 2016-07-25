private
createRingMethodForSelector: aSelector inClass: aClass
	"Create RingMethod from the class compiledMethod or if it was removed from the
	removealinfo"
	(aClass includesSelector: aSelector) ifTrue:[ ^ aClass compiledMethodAt: aSelector].
	^ self createRingMethodForRemovedSelector: aSelector inClass: aClass.