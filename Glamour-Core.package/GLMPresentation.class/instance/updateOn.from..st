updating
updateOn: anAnnouncement from: aBlockOrSymbol 
	updateActions add: (GLMSingleUpdateAction new 
		presentation: self;
		announcement: anAnnouncement; 
		transformation: aBlockOrSymbol;
		yourself)