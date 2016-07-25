file paths
winFontDirectories
	| directory |
	directory := self getWindowsFontFolderPath.
	directory ifNil: [ ^ #() ].
	directory := directory asFileReference.
	directory exists 
		ifTrue: [ ^ { directory }].
	^ #()