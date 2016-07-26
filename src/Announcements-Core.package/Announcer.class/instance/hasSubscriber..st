testing
hasSubscriber: anObject

	registry subscriptionsOf: anObject do: [:each | ^ true].
	^ false