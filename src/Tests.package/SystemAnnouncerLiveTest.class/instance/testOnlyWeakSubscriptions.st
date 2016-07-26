tests
testOnlyWeakSubscriptions

	"only weak subscriptions should be allowed to be added to the SystemAnnouncer..."

	
	"self assert: (
		(SystemAnnouncer uniqueInstance subscriptions instVarNamed: #subscriptions) 
			allSatisfy: [ :sub | sub isKindOf: WeakAnnouncementSubscription ]
	)"