interaction
handlesMouseDown: evt
	^ actionBlock notNil or: [super handlesMouseDown: evt]