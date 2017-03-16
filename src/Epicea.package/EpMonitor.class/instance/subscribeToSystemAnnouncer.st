private
subscribeToSystemAnnouncer

	{	CategoryAdded -> #categoryAdded:. 
		CategoryRemoved -> #categoryRemoved:.
		CategoryRenamed -> #categoryRenamed:.
		ClassAdded-> #behaviorAdded:.
		ClassRemoved->#behaviorRemoved:.
		MethodAdded -> #methodAdded:.
		MethodRemoved -> #methodRemoved:.
		ProtocolAdded -> #protocolAdded:.
		ProtocolRemoved -> #protocolRemoved:.
		ClassModifiedClassDefinition -> #behaviorModified:.
		MethodModified -> #methodModified:.
		ClassRecategorized -> #classRecategorized:.
		ClassRenamed -> #classRenamed:.
		ClassCommented -> #classCommented:.
		MethodRecategorized -> #methodRecategorized:.
		SnapshotDone -> #snapshotDone:. 
		MCVersionSaved	-> #monticelloVersionSaved:.
		
	} do: [ :pair |
		systemAnnouncer weak
			when: pair key 
			send: pair value 
			to: self ].
