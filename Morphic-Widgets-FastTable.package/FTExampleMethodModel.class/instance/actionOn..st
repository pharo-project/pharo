accessing
actionOn: aBrowser
	^ actionIcon ifNil: [ actionIcon := self createActionIconOn: aBrowser ]