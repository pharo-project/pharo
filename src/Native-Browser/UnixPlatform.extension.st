Extension { #name : #UnixPlatform }

{ #category : #'*Native-Browser' }
UnixPlatform >> privateOpenOSFileBrowserOn: aPathString [
	^ LibC runCommand: ('xdg-open "{1}"' format: {aPathString})
]
