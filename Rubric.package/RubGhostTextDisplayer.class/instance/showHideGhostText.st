private
showHideGhostText
	self textArea text
		ifNotEmpty: [ self removeAllMorphs ]
		ifEmpty: [ 
			self paragraphProvider configureGhostText: ghostText.
			self addMorph: ghostText ]