events handling
whenFindReplaceWindowRequired: anAnnouncement
	self findText
		ifEmpty: [ 
			self textArea hasSelection
				ifTrue: [ self findText: self textArea selection ] ].
	self dialog open