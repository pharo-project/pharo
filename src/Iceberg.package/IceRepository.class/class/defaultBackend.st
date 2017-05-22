accessing
defaultBackend
	^ self defaultBackendType
		ifNotNil: [ :className | 
			Smalltalk
				at: className
				ifAbsent: [ IceError signal: 'Selected Iceberg backend is missing: ', className ] ]		
		ifNil: [ 
			| backend | 
			backend := self detectDefaultBackend.
			self defaultBackend: backend.
			backend ]