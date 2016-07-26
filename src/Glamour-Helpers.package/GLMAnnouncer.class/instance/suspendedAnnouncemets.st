accessing
suspendedAnnouncemets
	^ suspendedAnnouncemets ifNil: [ suspendedAnnouncemets := OrderedCollection new ].