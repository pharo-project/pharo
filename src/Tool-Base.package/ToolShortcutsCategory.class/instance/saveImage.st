keymaps
saveImage
	<shortcut>
	^ KMKeymap
		shortcut: PharoShortcuts current saveImageShortcut
		action: [ 
			(Smalltalk snapshot: true andQuit: false)
				ifFalse: [ UIManager default inform: 'Image saved' ] ]