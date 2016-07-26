private
errorDefinitionWarning
	^ String streamContents:
		[:s |
		s nextPutAll: 'The following definitions had errors while loading.  Press Proceed to try to load them again (they may work on a second pass):'; cr.
		errorDefinitions do: [:ea | s space; space; nextPutAll: ea summary; cr]] 