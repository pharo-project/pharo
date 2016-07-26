accessing
summary
	^String streamContents: [:stream |
		self dependencies
			do: [:ea | stream nextPutAll: ea versionInfo name; cr ]]