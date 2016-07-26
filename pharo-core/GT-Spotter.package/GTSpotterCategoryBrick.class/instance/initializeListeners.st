initialization
initializeListeners
	self announcer when: #onMouseHover send: #onHovered to: self. 
	self announcer when: #onMouseUnhover send: #onUnhovered to: self