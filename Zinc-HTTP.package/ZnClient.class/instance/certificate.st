options
certificate
	"Return the TLS/SSL certificate to be used, or nil for none."
	
	^ self optionAt: #certificate ifAbsent: [ nil ]