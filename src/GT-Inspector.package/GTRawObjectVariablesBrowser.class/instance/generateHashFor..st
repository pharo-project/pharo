private - updating
generateHashFor: anObject 

	^ (self basicVariableValuePairsFor: anObject)
		inject: anObject gtInspectorHash
		into: [ :hashValue :assoc | hashValue bitXor: assoc value gtInspectorHash ].
	
	