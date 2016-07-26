building pane
computePane

	self hasUniquePane ifTrue: [^ self computePaneWith: self computeSinglePaneValidator].
	self atBeginning ifTrue: [^ self computePaneWith: self computeFirstPaneValidator].
	self atEnd ifTrue: [^ self computePaneWith: self computeLastPaneValidator].
	^( self computePaneWith: self computeMiddlePaneValidator).
	
