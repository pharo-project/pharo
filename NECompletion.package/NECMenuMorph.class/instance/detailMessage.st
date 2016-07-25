drawing
detailMessage

	^ String streamContents: [:stream |
		self flag: #todo. "Removing for now... most of the shortcuts are broken and no time or need now to fix them" 
		"stream << 'ctrl-h=help'."
		NECPreferences captureNavigationKeys ifTrue: [
			stream << (detailMorph
				ifNil: ['-> open detail']
				ifNotNil: ['<- close detail']	) ] ]