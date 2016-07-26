event handling
registerTextArea 
	textArea announcer  
	when: RubExtentChanged send: #whenExtentChanged: to: self;
	when: RubTextChanged send: #whenTextChanged: to: self