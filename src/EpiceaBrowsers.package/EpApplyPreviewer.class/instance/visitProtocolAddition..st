visitor
visitProtocolAddition: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^(behavior organization protocolOrganizer hasProtocolNamed: aChange protocol)
				ifTrue: [ #() ]
				ifFalse: [ { EpProtocolAddition behavior: behavior protocol: aChange protocol } ] ].

	^ #()