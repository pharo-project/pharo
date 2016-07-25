system announcements
classParentRenamed: anAnnouncement
	| class |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement classAffected.
	(self selectedClass = class)
		ifTrue:
			[ 
			self updateClassView.
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [ self updateSourceCodeAreaWith: self sourceCode ] ]