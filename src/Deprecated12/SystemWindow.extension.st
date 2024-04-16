Extension { #name : 'SystemWindow' }

{ #category : '*Deprecated12' }
SystemWindow class >> closeTopWindow [

	self
		deprecated: 'Please use #closeTopWindowByShortcut instead.'
		transformWith: '`@receiver closeTopWindow' -> '`@receiver closeTopWindowByShortcut'.

	^ self closeTopWindowByShortcut
]
