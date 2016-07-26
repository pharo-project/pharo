accessing
setContentDisposition: value
	^ self headers at: 'Content-Disposition' put: value 