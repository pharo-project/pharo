Extension { #name : #MacOSPlatform }

{ #category : #'*Native-Browser' }
MacOSPlatform >> privateOpenNativeBrowserOn: aPathString [
	^ LibC runCommand: ('open "{1}"' format: {aPathString})
]
