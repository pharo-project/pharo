private - updating
generateHashFor: anObject 

	^ (self basicVariableNodesFor: anObject)
		inject: anObject gtInspectorHash
		into: [ :hashValue :assoc | hashValue bitXor: assoc value gtInspectorHash ].
	
	