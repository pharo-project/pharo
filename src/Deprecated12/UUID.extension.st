Extension { #name : 'UUID' }

{ #category : '*Deprecated12' }
UUID >> size [

	self
		deprecated:
		'The UUIDs are no longer a subclass of ByteArray, and should not be directly modified.'
		transformWith: '`@receiver size' -> '`@receiver uuidData size'.
	^ 16
]
