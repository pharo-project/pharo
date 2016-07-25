announce requests
registerRequestHandlers
	self model announcer when: MorphTreeChangeRequest send: #changeRequest: to: self.