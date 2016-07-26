testing
lastIsReturn
	(self isInlineIf or: [ self isInlineIfNil]) ifFalse: [^false  ].
	arguments size = 1 ifTrue: [ ^false ].
	^ arguments first isBlock and: [ arguments first body lastIsReturn 
		and: [ arguments last isBlock and: [ arguments last body lastIsReturn ] ] ] 