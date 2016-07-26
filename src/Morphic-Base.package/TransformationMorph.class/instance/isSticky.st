testing
isSticky
	submorphs isEmpty ifFalse: [^ submorphs first isSticky].
	^false