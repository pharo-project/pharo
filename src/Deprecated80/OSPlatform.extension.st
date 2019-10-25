Extension { #name : #OSPlatform }

{ #category : #'*Deprecated80' }
OSPlatform >> openNativeBrowserOn: aFileReference [ 
	self deprecated: 'Use message openOSFileBrowserOn:' transformWith: '`@receiver openNativeBrowserOn: `@statements1' -> '`@receiver v `@statements1'.
	^ self openOSFileBrowserOn: aFileReference

]
