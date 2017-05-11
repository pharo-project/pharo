visitor
visitProtocolRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^(behavior organization protocolOrganizer hasProtocolNamed: aChange protocol)
				ifTrue: [ { EpProtocolRemoval behavior: behavior protocol: aChange protocol } ]
				ifFalse: [ #() ] ].

	^ #()