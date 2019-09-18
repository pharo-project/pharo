Extension { #name : #AbstractFileReference }

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInNativeBrowser [
	self deprecated: 'Use message openOSFileBrowserOn:' transformWith: '`@receiver openNativeBrowserOn: `@statements1' -> '`@receiver v `@statements1'.
	^ self openInOSFileBrowser

]

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInOSFileBrowser [
	^ OSPlatform current openOSFileBrowserOn: self
]
