private
userFrom: event
	^ [ event request basicAuthentication first ]
		on: Error
		do: [ '-' ]