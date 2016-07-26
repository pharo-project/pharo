accessing
pluginVersion
	"Returns the version of the plugin"

	^ self primitiveSSL: handle getIntProperty: 0