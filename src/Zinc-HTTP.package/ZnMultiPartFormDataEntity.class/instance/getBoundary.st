accessing
getBoundary
	"Return the multipart/form-data MIME part boundary.
	This should really be there, set externally or internally.
	See #initialize and #mimeTypeWithBoundary"
	
	^ (self contentType parameterAt: 'boundary') withoutQuoting 