instance creation
uniqueInstance
	^ uniqueInstance first ifNil: [ 
		uniqueInstance at: 1 put: (
			self new
				occupant: ReSystemAnnouncer uniqueInstance;
				register;
				yourself) ]