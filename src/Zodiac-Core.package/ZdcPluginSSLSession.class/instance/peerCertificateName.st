accessing
peerCertificateName
	"Returns the certificate name of the remote peer.
	The method only returns a name if the certificate has been verified"

	^ self primitiveSSL: handle getStringProperty: 0