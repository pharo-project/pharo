Extension { #name : #AbstractFileReference }

{ #category : #'*Native-Browser' }
AbstractFileReference >> openInOSFileBrowser [
	^ NativeBrowserOpenVisitor openOn: self
]
