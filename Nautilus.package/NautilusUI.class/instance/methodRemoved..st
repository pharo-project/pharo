system announcements
methodRemoved: anAnnouncement
	| method |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	method := anAnnouncement method.
	(self interestedInClass: method methodClass)
		ifTrue: [ 
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [self selectNextMethod].
			self removeAllFromMethodsIconsCache: method.
			method isTestMethod
				ifTrue:
					[self updateClassView ].	"Should update class icon"
			self resetMethodsList.
			methodWidget updateMethodList.
			(method methodClass isMeta and: [ method methodClass hasMethods not ])
				ifTrue: [ self changed: #instanceButtonLabel ] ]