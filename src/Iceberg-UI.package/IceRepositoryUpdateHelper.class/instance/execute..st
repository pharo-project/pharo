executing
execute: ann
	self presentation
		updateOn: IceRepositoryAnnouncement 
		from: [ ann repository announcer weak ]