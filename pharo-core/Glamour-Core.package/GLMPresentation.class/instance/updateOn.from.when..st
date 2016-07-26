updating
updateOn: anAnnouncement from: aBlockOrSymbol when: aConditionBlock
	updateActions add: (GLMSingleUpdateAction new
		presentation: self; 
		announcement: anAnnouncement; 
		transformation: aBlockOrSymbol;
		condition: aConditionBlock;
		yourself)