running
basicCheck: aMethod 
	| ownerProtocol |
	ownerProtocol := aMethod protocol.
	ownerProtocol ifNil: [ ^ false ].
	(ownerProtocol == #'as yet unclassified' or: [ 
	 ownerProtocol first = $*	 ]) ifTrue: [ ^ false ].
	
	aMethod methodClass superclass ifNotNil: [ :superclass |
			(superclass lookupSelector: aMethod selector) ifNotNil: [ :superMethod |
				| superProtocol |
				superProtocol := superMethod protocol.
				(superProtocol isNil or: [ 
			 	 superProtocol == #'as yet unclassified' or: [ 
			 	 superProtocol first = $*	 ] ]) ifFalse: [ 
					^ ownerProtocol ~= superProtocol ] ] ].
	
	^ false