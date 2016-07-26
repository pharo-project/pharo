origins
userData
	^ self directoryFromEnvVariableNamed: 'XDG_DATA_HOME' or: [ self home / '.local' / 'share' ]