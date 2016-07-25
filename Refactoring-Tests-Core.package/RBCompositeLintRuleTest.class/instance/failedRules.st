accessing
failedRules
	^rules inject: OrderedCollection new into: [:oc :each | oc addAll: each failedRules; yourself]