Extension { #name : #AbstractFileReference }

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInNativeBrowser [
	^ OSPlatform current openNativeBrowserOn: self
]
