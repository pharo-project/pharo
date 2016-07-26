operations
authorString
	| authorName |
	authorName := self log authorAt: entry ifAbsent: [ ^ '' ].
	
	^ authorName = EpMonitor current author
		ifTrue: [ '' ]
		ifFalse: [ authorName ]