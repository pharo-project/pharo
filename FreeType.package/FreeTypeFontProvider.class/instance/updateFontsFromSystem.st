loading and updating
updateFontsFromSystem
	| done |
	"Add all the embedded file infos"
	FT2Library current ifNil: [ ^ self ].
	self updateEmbeddedFreeTypeFonts.
	done := Set new.	"visited directories are tracked in done, so that they are not processed twice"
	self platformImageRelativeDirectories
		do: [ :each | self updateFromDirectory: each done: done ]
		displayingProgress: 'Loading image relative font files'.
	self platformVMRelativeDirectories
		do: [ :each | self updateFromDirectory: each done: done ]
		displayingProgress: 'Loading vm relative font files'.
	self platformAbsoluteDirectories
		do: [ :each | self updateFromDirectory: each done: done ]
		displayingProgress: 'Loading platform font files'