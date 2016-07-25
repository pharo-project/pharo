protocol - removing
removeProtocol: aProtocol
	
	aProtocol canBeRemoved
		ifFalse: [ 
			"Virtual protocols who cannot be removed should not raise an error"
			aProtocol isVirtualProtocol 
				ifTrue: [ ^ self ].
			ProtocolRemovalException signal ].
	
	^ protocols 
		remove: aProtocol
		ifAbsent: [ ]