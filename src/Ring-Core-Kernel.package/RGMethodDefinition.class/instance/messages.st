backward compatibility
messages
	self isActive
		ifTrue: [ ^ self method messages ].
	^ self ast ifNotNil: [ :ast | ast sentMessages ] ifNil: [ #() ]