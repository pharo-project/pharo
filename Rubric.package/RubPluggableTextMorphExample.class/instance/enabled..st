accessing
enabled: aBoolean
	enabled = aBoolean ifTrue: [ ^self ].
	enabled := aBoolean.
	self changed: #enabled