system startup
shutDown
	World ifNotNil: [:world | world triggerEvent: #aboutToLeaveWorld ].
	self extraWorldList do: [:world | world triggerEvent: #aboutToLeaveWorld ].