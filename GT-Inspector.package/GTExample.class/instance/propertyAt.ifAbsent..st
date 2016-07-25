public
propertyAt: aKey ifAbsent: aBlock
	^ (properties ifNil: [ ^ aBlock value ])
		at: aKey ifAbsent: aBlock