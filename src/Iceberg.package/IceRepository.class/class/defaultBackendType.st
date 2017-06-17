accessing settings
defaultBackendType
	^ DefaultBackendType ifNil: [ DefaultBackendType := #IceLibgitLocalRepository ]
