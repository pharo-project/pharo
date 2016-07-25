taskbar-private
customizeTaskbarButton: aButton

	self taskbarCanShowThumbnail ifFalse: [ ^ self ].
	
	aButton 
	
		on: #mouseEnter 
		send: #taskbarButtonEntered:event:in: 
		to: self 
		withValue: aButton;
		
		on: #mouseLeave 
		send: #taskbarButtonLeft:event:in: 
		to: self  
		withValue: aButton.