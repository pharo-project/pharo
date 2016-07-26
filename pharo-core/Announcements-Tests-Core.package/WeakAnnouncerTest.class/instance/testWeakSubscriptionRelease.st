testing
testWeakSubscriptionRelease
	"This test shows the a problem with weak registrations not garbage collected in Spur.
	See https://pharo.fogbugz.com/f/cases/17537/SystemAnnouncer-has-far-too-many-subscriptions for more details."
	self benchManyWeakGuys