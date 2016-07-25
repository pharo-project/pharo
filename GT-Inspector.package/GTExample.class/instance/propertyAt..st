public
propertyAt: aKey
	^ (properties ifNil: [ KeyNotFound signalFor: aKey ])
		at: aKey