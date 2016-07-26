editing
copyToClipboard
	
	controller selection isEmpty ifTrue: [ ^ self ].

	
	Clipboard clipboardText: controller selection asString.
	
