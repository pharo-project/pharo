class building
installingIn: aClass
	super installingIn: aClass.
	offset := self calculateOffset: aClass.