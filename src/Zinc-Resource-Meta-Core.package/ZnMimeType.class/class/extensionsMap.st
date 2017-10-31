private
extensionsMap
	ExtensionsMap ifNil: [ self initializeExtensionsMap ].
	^ ExtensionsMap