Extension { #name : #MacOSPlatform }

{ #category : #'*Native-Browser' }
MacOSPlatform >> privateOpenOSFileBrowserOn: aPathString [
	^ LibC runCommand: ('open "{1}"' format: {aPathString})
]
