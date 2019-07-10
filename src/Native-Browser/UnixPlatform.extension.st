Extension { #name : #UnixPlatform }

{ #category : #'*Native-Browser' }
UnixPlatform >> privateOpenNativeBrowserOn: aPathString [
	^ LibC runCommand: ('xdg-open "{1}"' format: {aPathString})
]
