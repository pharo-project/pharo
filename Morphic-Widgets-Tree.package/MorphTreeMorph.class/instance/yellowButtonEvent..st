events-processing
yellowButtonEvent: anEvent

	(self scrollerSubMorphFromPoint: anEvent position) 
		ifNotNil: [:sel |
			sel selected
				ifFalse: [self listManager setSelectedMorph: sel].
			^ self yellowButtonActivity: anEvent shiftPressed ].
	^ self yellowButtonActivity: anEvent shiftPressed