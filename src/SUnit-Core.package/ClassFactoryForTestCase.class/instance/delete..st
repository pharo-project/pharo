cleaning
delete: aBehavior
	aBehavior isObsolete
		ifTrue: [ ^ self ].
	self class environment at: #ChangeSet ifPresent: [ 
		aBehavior removeFromChanges ].
	(createdSilently includes: aBehavior)
		ifTrue: [ aBehavior removeFromSystemUnlogged ]
		ifFalse: [ aBehavior removeFromSystem ].