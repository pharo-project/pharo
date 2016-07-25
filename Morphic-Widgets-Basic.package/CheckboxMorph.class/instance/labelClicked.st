event handling
labelClicked

	labelClickable ifTrue: [ self toggleSelected ].
	
	self doAnnounce: (LabelClicked source: self stateChanged: labelClickable).