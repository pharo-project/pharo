Extension { #name : #AbstractFileReference }

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInOSFileBrowser [
	^ OSPlatform current openOSFileBrowserOn: self
]
