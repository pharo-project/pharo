event handling
registerTextArea
	super registerTextArea.
	
	textArea announcer  
		when: RubKeystroke send: #whenKeyPressed: to: self;
		when: RubMouseClick send: #whenMouseClicked: to: self;
		when: RubSelectionChanged send: #whenSelectionChanged: to: self.
	
	self color: (self theme rubScrolledTextMorphThemer highlightBackgroundColorFor: textArea).
	self borderStyle: (self theme rubScrolledTextMorphThemer highlightBorderStyleFor: textArea).
	self borderWidth: (self textArea hasSelection
		ifTrue: [ 2 * self borderWidth] 
		ifFalse: [ self borderWidth ])