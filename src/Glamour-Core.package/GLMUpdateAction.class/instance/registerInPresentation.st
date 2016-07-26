public
registerInPresentation
	self announcerObjects do: [: announcerObject |
		announcerObject notNil ifTrue: [
			(announcerObject when: self announcement send: #actOn: to: self) makeWeak.
			
			"we remember the announcer object to be able to 
			unregister from it when the presentation goes away"
			self presentation registeredAnnouncers add: announcerObject ] ]