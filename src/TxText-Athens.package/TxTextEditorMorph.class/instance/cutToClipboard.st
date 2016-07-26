editing
cutToClipboard
	
	controller selection isEmpty ifTrue: [ ^ self ].

	Clipboard clipboardText: controller selection asString.
	self handleDeleteKey.
