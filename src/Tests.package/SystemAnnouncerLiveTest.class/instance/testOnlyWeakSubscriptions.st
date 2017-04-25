tests
testOnlyWeakSubscriptions

	| strong |

	"only weak subscriptions should be allowed to be added to the SystemAnnouncer..."
	
	strong := SystemAnnouncer uniqueInstance 
		subscriptions subscriptions reject: [ :each |
			each isKindOf: WeakAnnouncementSubscription
		].
	
	self assert: strong asArray equals: #()