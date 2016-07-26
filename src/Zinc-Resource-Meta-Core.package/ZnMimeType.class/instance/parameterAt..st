parameters
parameterAt: key
	^ self parameterAt: key ifAbsent: [ KeyNotFound signalFor: key ]