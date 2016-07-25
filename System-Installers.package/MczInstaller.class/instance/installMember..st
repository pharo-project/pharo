installation
installMember: member
	 
	self useNewChangeSetDuring:
		[ CodeImporter evaluateReadStream: (self contentsForMember: member) readStream.
		]