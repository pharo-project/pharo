accessing
sharedPoolNames
	^ self sharedPools collect: [:ea |
		ea isObsolete
			ifTrue: [ ea name ]
			ifFalse: [ self environment keyAtIdentityValue: ea ] ]