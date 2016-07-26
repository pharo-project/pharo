private
closeConnections
	self connections isEmpty ifTrue: [ ^ self ].
	self lock critical: [
		self connections do: [ :each | 
			self logConnectionClosed: each.
			[ each close ] 
				on: Exception 
				do: [ ] ].
		self connections removeAll ]