system announcements
classRenamed: anAnnouncement
	| class |
	class := anAnnouncement classRenamed.
	self setWindowTitle.
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	(self classWidget getClassesList includes: class)
		ifTrue:
			[ 
			self updateClassView.
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [ self updateSourceCodeAreaWith: self sourceCode ] ]