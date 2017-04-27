accessing
serverName
	"Get the Server Name Indication (SNI) property"

	^ self primitiveSSL: handle getStringProperty: 2