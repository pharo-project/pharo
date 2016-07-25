system announcements
classAdded: anAnnouncement 
	| class |
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	class := anAnnouncement classAdded.
	((self parentOfClass: class) nodePath 
		anySatisfy: [ :node | node item = class package ]) 
		ifTrue: [ 
			self updateClassView.
			self removeFromPackagesIconsCache: class package ]
			