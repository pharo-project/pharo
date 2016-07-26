protocol
getTextFor: aMethod

	method := aMethod.
	method ifNil: [ ^ '' ].
	
	^ self internalGetText