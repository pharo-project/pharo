Extension { #name : #OSPlatform }

{ #category : #'*Deprecated80' }
OSPlatform >> openNativeBrowserOn: aFileReference [ 
	self deprecated: 'Use message openOSFileBrowserOn:'.
	^ self openOSFileBrowserOn: aFileReference

]
