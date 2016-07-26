accessing
at: aKey
	^ self at: aKey ifAbsent: [ KeyNotFound signalFor: aKey ]