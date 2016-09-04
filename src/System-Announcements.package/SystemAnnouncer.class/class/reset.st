initialization
reset

	"unregister all current weak subscriptions because it can cause a memory leak"
	announcer subscriptions glmSubscriptions
 		select: [:each | each isKindOf: LegacyWeakSubscription] 
		thenDo: [:each | each weakRegistry remove: each subscriber ifAbsent: []]. 
	Smalltalk garbageCollect.
		
	announcer := nil.