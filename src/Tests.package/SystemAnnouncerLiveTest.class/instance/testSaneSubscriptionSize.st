tests
testSaneSubscriptionSize
	
	self flag: 'this should be a release test, not a regular one'
	"self assert: SystemAnnouncer uniqueInstance subscriptions numberOfSubscriptions < 75."