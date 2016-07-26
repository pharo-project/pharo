accessing
source
	^ String streamContents:
		[:s |
		categories do: [:ea | s nextPutAll: ea] separatedBy: [s cr]]