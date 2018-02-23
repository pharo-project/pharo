Extension { #name : #SubscriptionRegistry }

{ #category : #'*Glamour-Tests-Core' }
SubscriptionRegistry >> glmSubscriptions [

	self
		deprecated: 'Please use #subscriptions instead'
		transformWith: '`@receiver glmSubscriptions' -> '`@receiver subscriptions'.
	^ subscriptions
]
