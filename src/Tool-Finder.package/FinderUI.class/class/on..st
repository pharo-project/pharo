instance creation
on: aFinder
	| instance |
	instance := self new finder: aFinder.
	self doAllSubscriptionsOn: aFinder to: instance.
	^instance.