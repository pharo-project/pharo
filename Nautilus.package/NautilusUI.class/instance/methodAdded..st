system announcements
methodAdded: anAnnouncement
	
	| method |
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	method := anAnnouncement method.
	(self interestedInClass: method methodClass)
		ifTrue: [ 
			self resetMethodsListSelection.
			self removeAllFromMethodsIconsCache: method.
			self updateCategoryAndMethodList.
			(method methodClass isMeta and: [ method methodClass methods size = 1 ])
				ifTrue: [ self changed: #instanceButtonLabel ] ]