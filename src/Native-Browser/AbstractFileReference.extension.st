Extension { #name : #AbstractFileReference }

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInNativeBrowser [
	self deprecated: 'Use message openInOSFileBrowser'.
	^ self openInOSFileBrowser

]

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInOSFileBrowser [
	^ OSPlatform current openOSFileBrowserOn: self
]
