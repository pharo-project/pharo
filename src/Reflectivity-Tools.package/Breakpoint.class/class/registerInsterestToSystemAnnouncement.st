class initialization
registerInsterestToSystemAnnouncement
	<systemEventRegistration>
	SystemAnnouncer uniqueInstance
		unsubscribe: self.
	SystemAnnouncer uniqueInstance weak subscribe: MethodRemoved send: #handleMethodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak subscribe: ClassRemoved send: #handleClassRemoved: to: self.