updating
updateOn: anAnnouncement fromAll: aBlockOrSymbol 
	updateActions add: (GLMMultipleUpdateAction new
		presentation: self; 
		announcement: anAnnouncement; 
		transformation: aBlockOrSymbol;
		yourself)