Extension { #name : #PasteUpMorph }

{ #category : #'*Polymorph-Widgets' }
PasteUpMorph >> backgroundImage: aForm layout: aSymbol [
	"Set a background image, replacing the current background morph, if any,
	with the given layout (see AlphaImageMorph layoutSymbols). "

	self backgroundMorph: (
		(self theme builder
			newAlphaImage: aForm help: nil)
			autoSize: false;
			layout: aSymbol;
			lock)
]

{ #category : #'*Polymorph-Widgets' }
PasteUpMorph >> navigationKey: aKeyboardEvent [
	
	^ self taskList handleEvent: aKeyboardEvent.
]

{ #category : #'*Polymorph-Widgets' }
PasteUpMorph >> themeChanged [
	"The theme has changed.
	Update the desktop wallpaper if appropriate."

	(self theme desktopImageFor: self) ifNotNil: [:aForm |
		self color: Color white.
		self backgroundImage: aForm layout: self theme desktopImageLayout].
	super themeChanged
]
