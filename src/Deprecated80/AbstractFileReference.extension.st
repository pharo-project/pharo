Extension { #name : #AbstractFileReference }

{ #category : #'*Deprecated80' }
AbstractFileReference >> openInNativeBrowser [
	self deprecated: 'Use message openInOSFileBrowser' transformWith: '`@receiver openNativeBrowserOn' -> '`@receiver openInOSFileBrowser'.
	^ self openInOSFileBrowser
]
