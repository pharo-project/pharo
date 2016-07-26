accessing
certificateName
	"The name of the local certificate to provide to the remote peer"

	^ self primitiveSSL: handle getStringProperty: 1