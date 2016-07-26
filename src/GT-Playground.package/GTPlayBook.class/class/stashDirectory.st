accessing
stashDirectory
	" lazy initialize the stash and always ensure its existence - especially while the image is running "
	(stashDirectory notNil and: [ stashDirectory exists ])
		ifFalse: [ stashDirectory := self defaultStashDirectory ].
	^ stashDirectory ensureCreateDirectory