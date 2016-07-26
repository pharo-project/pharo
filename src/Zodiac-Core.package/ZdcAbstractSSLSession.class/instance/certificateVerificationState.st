accessing
certificateVerificationState
	"Returns the certificate verification bits. The returned value indicates
	whether the certificate is valid. The two standard values are:
		0   - The certificate is valid
		-1  - No certificate has been provided by the peer
	Otherwise, the result is a bit mask of the following values:
		1   - If set, there is an unspecified issue with the cert (generic error)
		2   - If set, the root CA is untrusted (usually a self-signed cert)
		4   - If set, the certificate is expired
		8   - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid
		32 - If set, the certificate was revoked"

	self subclassResponsibility