initialization
initializeAnnouncements
	SystemAnnouncer uniqueInstance weak 
		when:
			ClassAdded , ClassCommented , ClassRecategorized , ClassModifiedClassDefinition , ClassRemoved , ClassRenamed
				, ClassReorganized , MethodAdded , MethodModified , MethodRecategorized , MethodRemoved , ProtocolAdded
				, ProtocolRemoved
		send: #updateClassesList
		to: self