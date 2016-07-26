initialize-release
connectToChanges
	SystemAnnouncer uniqueInstance weak
		when: CategoryAdded,
			CategoryRemoved,
			CategoryRenamed,
			ClassAdded,
			ClassModifiedClassDefinition,
			ClassRemoved,
			ClassRenamed,
			ClassReorganized,
			MethodAdded,
			MethodModified,
			MethodRemoved,
			ProtocolAdded,
			ProtocolRemoved
		send: #update:
		to: self.