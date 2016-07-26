accessing
renameProtocol: oldName into: newName
	(self existsProtocolNamed: newName) 
		ifTrue: [ 
			self moveMethodsFrom: oldName to: newName.
			self removeProtocolNamed: oldName ] 
		ifFalse: [ 
			^ (self protocolNamed: oldName)
				name: newName;
				yourself ].