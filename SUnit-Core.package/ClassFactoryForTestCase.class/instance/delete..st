cleaning
delete: aBehavior
	aBehavior isObsolete
		ifTrue: [ ^ self ].
	aBehavior removeFromChanges.
	(createdSilently includes: aBehavior)
		ifTrue: [ aBehavior removeFromSystemUnlogged ]
		ifFalse: [ aBehavior removeFromSystem ].