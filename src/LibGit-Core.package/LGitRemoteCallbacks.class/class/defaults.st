instance creation
defaults
	^ super defaults 
		certificateCheck: self newCertificateCheckCallback;
		yourself