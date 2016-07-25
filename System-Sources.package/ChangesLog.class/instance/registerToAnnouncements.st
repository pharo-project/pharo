event-listening
registerToAnnouncements
	SystemAnnouncer uniqueInstance weak
		when: ClassRemoved 
			send: #logClassRemoved:
			to: self;
		when: ClassRenamed 
			send: #logClassRenamed:
			to: self;
		when: MethodRemoved
			send: #logMethodRemoved:
			to: self;
		when: ExpressionEvaluated 
			send: #logExpressionEvaluated:
			to: self.