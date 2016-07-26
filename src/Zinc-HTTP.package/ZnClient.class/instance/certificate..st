options
certificate: anObject
	"Set the TLS/SSL certificate to be used to anObject, or nil for none.
	See ZdcAbstractSSLSession>>#certificateName: for more information."
	
	^ self optionAt: #certificate put: anObject