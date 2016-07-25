dropping/grabbing
formerPosition: formerPosition 
	formerPosition ifNil: [self removeProperty: #formerPosition]
		ifNotNil: [self setProperty: #formerPosition toValue: formerPosition]