initialization
fromAnnouncement: anAnnouncement

	currentTextInfo := self textInfoFrom: anAnnouncement currentText.
	anAnnouncement previousText size == 0 ifTrue: [ ^ self ].
	previousTextInfo := self textInfoFrom: anAnnouncement previousText.
	isRefinement := (anAnnouncement currentText size > anAnnouncement previousText size 
		ifTrue: [ anAnnouncement currentText beginsWith: anAnnouncement previousText]
		ifFalse: [ anAnnouncement previousText beginsWith: anAnnouncement currentText ]).