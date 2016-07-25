settings
useNetworkAuthentification
	"Should authentication be used when accessing the HTTP proxy ?
	This is a switch over username/password."

	^ UseNetworkAuthentification ifNil: [ UseNetworkAuthentification := false ]