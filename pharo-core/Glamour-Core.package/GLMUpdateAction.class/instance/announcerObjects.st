accessing
announcerObjects 
	^ announcerObjects := self computeAnnouncerObjects ifNil: [OrderedCollection new]