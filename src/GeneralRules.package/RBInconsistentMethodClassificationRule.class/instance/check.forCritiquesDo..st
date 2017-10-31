running
check: aMethod forCritiquesDo: aCritiqueBlock
	| ownerProtocol |
	ownerProtocol := aMethod protocol.
	ownerProtocol ifNil: [ ^ self ].
	(ownerProtocol == #'as yet unclassified' or: [ 
	 ownerProtocol first = $*	 ]) ifTrue: [ ^ self ].
	
	aMethod methodClass superclass ifNotNil: [ :superclass |
			(superclass lookupSelector: aMethod selector) ifNotNil: [ :superMethod |
				| superProtocol |
				superProtocol := superMethod protocol.
				(superProtocol isNil or: [ 
			 	 superProtocol == #'as yet unclassified' or: [ 
			 	 superProtocol first = $*	 ] ]) ifFalse: [ 
					ownerProtocol ~= superProtocol ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aMethod) ]] ] ].