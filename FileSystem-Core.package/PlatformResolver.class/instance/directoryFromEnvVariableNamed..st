private
directoryFromEnvVariableNamed: aString
	^ self directoryFromEnvVariableNamed: aString or: [ self cantFindOriginError ]