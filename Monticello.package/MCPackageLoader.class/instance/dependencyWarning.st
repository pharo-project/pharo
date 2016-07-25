private
dependencyWarning
	^ String streamContents:
		[:s |
		s nextPutAll: 'This package depends on the following classes:'; cr.
		requirements do: [:ea | s space; space; nextPutAll: ea; cr].
		s nextPutAll: 'You must resolve these dependencies before you will be able to load these definitions: '; cr.
		unloadableDefinitions do: [:ea | s space; space; nextPutAll: ea summary; cr]] 