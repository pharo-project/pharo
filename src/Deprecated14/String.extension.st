Extension { #name : 'String' }

{ #category : '*Deprecated14' }
String >> translated [

	self
		deprecated:
		'This method will be removed from the next Pharo version. It used to do nothing, just return self'
		transformWith: '`@receiver translated' -> '`@receiver '.
	^ self
]
