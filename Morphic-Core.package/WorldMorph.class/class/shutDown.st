system startup
shutDown
	World ifNotNil: [:world | world triggerEvent: #aboutToLeaveWorld ]