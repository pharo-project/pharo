accessing
current
	Current ifNil: [Current := SystemVersion new].
	^Current