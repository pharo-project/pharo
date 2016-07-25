accessing
contentDisposition
	^ self headers at: 'Content-Disposition' ifAbsent: [ nil ]