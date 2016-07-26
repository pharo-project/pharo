utilities
hasVersion: aString
	^ self allManagers anySatisfy: [ :version | version ancestry ancestorString = aString ]