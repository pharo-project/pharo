accessing
default
	^DefaultInstance ifNil: [ 
		DefaultInstance := self new.
		self registerInterestToSystemAnnouncement.
		DefaultInstance.].