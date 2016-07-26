accessing
certificateName: aString
	"Sets the name of the local certificate to provide to the remote peer.
	OpenSSL: The name is the full path to a .pem file.
	WinSSL: The name is matched against the 'subject' of a certificate in the cert store"

	self subclassResponsibility