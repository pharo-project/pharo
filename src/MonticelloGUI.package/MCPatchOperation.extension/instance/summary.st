*MonticelloGUI
summary
	| attribute |
	attribute := 
		self isResolved
			ifTrue: [self remoteChosen ifTrue: [#underlined] ifFalse: [#struckOut]]
			ifFalse: [#bold].
	^ Text string:( self definition summary, self summarySuffix) attribute: (TextEmphasis perform: attribute)