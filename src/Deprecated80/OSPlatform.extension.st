Extension { #name : #OSPlatform }

{ #category : #'*Deprecated80' }
OSPlatform >> openNativeBrowserOn: aFileReference [
	self
		deprecated: 'Use message NativeBrowserOpenVisitor openOn: instead'
		transformWith: '`@receiver openNativeBrowserOn: `@arg' -> 'NativeBrowserOpenVisitor openOn: `@arg'.
	^ NativeBrowserOpenVisitor openOn: aFileReference
]
